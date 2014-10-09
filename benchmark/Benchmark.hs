{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Trans
import Data.Time
import Database.Redis
import Text.Printf
import Network (HostName, PortID, PortNumber)
import System.Environment (getArgs)

nRequests, nClients :: Int
nRequests = 100000
nClients  = 50

-- | Datatype for command-line arguments.
data BArgs = HostAndPort HostName PortNumber | Host HostName | Neither

-- | Compute command-line arguments from the real world.
getBArgs :: IO BArgs
getBArgs = do
  args <- getArgs
  return $ case args of
    host : port : _ -> readHostAndPort host port 
    host : [] -> Host host
    _ -> Neither

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
    conn <- connect connectInfo
    runRedis conn $ do
        _ <- flushall
        Right _ <- mset [ ("k1","v1"), ("k2","v2"), ("k3","v3")
                        , ("k4","v4"), ("k5","v5") ]
    
        return ()
    
    let timeAction name nActions action = do
        startT <- getCurrentTime
        -- each clients runs ACTION nRepetitions times
        let nRepetitions = nRequests `div` nClients `div` nActions

        let replicatedAction = replicateM_ nRepetitions action

        -- One replicatedAction for each client.
        let actions = take nClients $ repeat replicatedAction

        -- Run each of the actions in a new runRedis, against the same
        -- connection.
        mapConcurrently (runRedis conn) actions

        stopT <- getCurrentTime
        let deltaT     = realToFrac $ diffUTCTime stopT startT
            -- the real # of reqs send. We might have lost some due to 'div'.
            actualReqs = nRepetitions * nActions * nClients
            rqsPerSec  = fromIntegral actualReqs / deltaT :: Double
        putStrLn $ printf "%-20s %10.2f Req/s" (name :: String) rqsPerSec

    ----------------------------------------------------------------------
    -- Benchmarks
    --
    timeAction "ping" 1 $ do
        Right Pong <- ping
        return ()
        
    timeAction "get" 1 $ do
        Right Nothing <- get "key"
        return ()
    
    timeAction "mget" 1 $ do
        Right vs <- mget ["k1","k2","k3","k4","k5"]
        let expected = map Just ["v1","v2","v3","v4","v5"]
        True <- return $ vs == expected
        return ()
    
    timeAction "ping (pipelined)" 100 $ do
        pongs <- replicateM 100 ping
        let expected = replicate 100 (Right Pong)
        True <- return $ pongs == expected
        return ()

    timeAction "multiExec get 1" 1 $ do
        TxSuccess _ <- multiExec $ get "foo"
        return ()
    
    timeAction "multiExec get 50" 50 $ do
        TxSuccess 50 <- multiExec $ do
                            rs <- replicateM 50 (get "foo")
                            return $ fmap length (sequence rs)
        return ()

    timeAction "multiExec get 1000" 1000 $ do
        TxSuccess 1000 <- multiExec $ do
                            rs <- replicateM 1000 (get "foo")
                            return $ fmap length (sequence rs)
        return ()
    