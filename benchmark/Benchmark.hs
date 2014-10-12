{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Redis
import Control.Monad
import Network (HostName, PortID, PortNumber)
import System.Environment (getEnv)
import Control.Exception
import Criterion.Main

nRequests, nClients :: Int
nRequests = 100000
nClients  = 50

-- | Datatype for command-line arguments.
data BArgs = HostAndPort HostName PortNumber | Host HostName | Neither

-- | Compute command-line arguments from the real world.
getBArgs :: IO BArgs
getBArgs = do
  host <- catch (getEnv "HEDIS_BENCH_HOST") (\x -> let _ = x :: SomeException in return "")
  port <- catch (getEnv "HEDIS_BENCH_PORT") (\x -> let _ = x :: SomeException in return "")
  case (null host, null port) of
    (True, True) -> return Neither
    (False, True) -> return $ Host host
    (False, False) -> return $ readHostAndPort host port
    -- You can't set the port but not the host; sorry.
    (True, False) -> return Neither

  where readHostAndPort host port = let (n, r) : _ = reads port in
            if r == "" then Host host else HostAndPort host (toEnum n)

-- | Compute ConnectInfo from the real world.
getConnectInfo :: IO ConnectInfo
getConnectInfo = getBArgs >>= return . makeConnectInfo
  where
    makeConnectInfo :: BArgs -> ConnectInfo
    makeConnectInfo Neither = defaultConnectInfo
    makeConnectInfo (Host hn) = defaultConnectInfo { connectHost = hn }
    makeConnectInfo (HostAndPort hn pn) = defaultConnectInfo {
          connectHost = hn
        , connectPort = PortNumber pn
        }

main :: IO ()
main = do

    connectInfo <- getConnectInfo

    ----------------------------------------------------------------------
    -- Preparation
    --
    -- Here we clear the database and set 5 values.
    --
    conn <- connect connectInfo
    runRedis conn $ do
        _ <- flushall
        Right _ <- mset [ ("k1","v1"), ("k2","v2"), ("k3","v3")
                        , ("k4","v4"), ("k5","v5") ]
    
        return ()

    ----------------------------------------------------------------------
    -- Benchmarks
    --
    -- Here we try our best to define actions such that, when run, the only
    -- work that will be performed is the work necessary to communicate with
    -- redis via hedis.
    --

    let getOnceNoResponse = runRedis conn $ get "key"

    let getOnceResponse = runRedis conn $ do
        Right Nothing <- get "key"
        return ()

    let getManyNoResponse = runRedis conn $ mget ["k1","k2","k3","k4","k5"]

    let getManyResponse = runRedis conn $ do
        Right vs <- mget ["v1", "v2", "v3", "v4", "v5"]
        -- We don't inspect the list because that's not relevant to the benchmark.
        -- We wish only to know that the value was pulled back from the server.
        return ()

    let multiExecNoResponse = runRedis conn $ do
        multiExec $ get "key"
        return ()

    let multiExecResponse = runRedis conn $ do
        TxSuccess (Nothing) <- multiExec $ get "key" >>= return
        return ()

    let multiExec50 = runRedis conn $ do
        TxSuccess 50 <- multiExec $ do
                            rs <- replicateM 50 (get "foo")
                            return $ fmap length (sequence rs)
        return ()

    let multiExec1000 = runRedis conn $ do
        TxSuccess 1000 <- multiExec $ do
                            rs <- replicateM 1000 (get "foo")
                            return $ fmap length (sequence rs)
        return ()


    let mixed0 = runRedis conn $ do
        set "key0" "value0"
        set "key1" "value1"
        set "key2" "value2"
        set "key3" "value3"
        set "key4" "value4"
        Right (Just k0) <- get "key0"
        Right (Just k4) <- get "key4"
        return ()

    -- Here we define the criterion benchmarks using the above actions.
    let pingGroup = bgroup "ping" [
            bench "once_no_response" $ whnfIO (runRedis conn ping)
          , bench "once_response" $ whnfIO (runRedis conn $ ping >>= \(Right Pong) -> return ())
          ]

    let getGroup = bgroup "get" [
            bench "once_no_response" $ whnfIO getOnceNoResponse
          , bench "once_response" $ whnfIO getOnceResponse
          , bench "many_no_response" $ whnfIO getManyNoResponse
          , bench "many_response" $ whnfIO getManyResponse
          ]

    let multiExecGroup = bgroup "multiexec" [
            bench "no_response" $ whnfIO multiExecNoResponse
          , bench "response" $ whnfIO multiExecResponse
          , bench "50" $ whnfIO multiExec50
          , bench "1000" $ whnfIO multiExec1000
          ]

    let mixedGroup = bgroup "mixed" [
            bench "mixed0" $ whnfIO mixed0
          ]


    let benchmarkGroups = [
            pingGroup
          , getGroup
          , multiExecGroup
          , mixedGroup
          ]

    defaultMain benchmarkGroups
