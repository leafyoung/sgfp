module Main ( 
    main
  , countSeqHandler
) where

import System.Environment
import Control.Concurrent
import Control.Concurrent.MVar 

import Network
import CountSeq

countSeqHandler :: MVar SeqInfo -> HandlerFunc
countSeqHandler mcseq addr msg = do
  (cA, cT, cG, cC) <- readMVar mcseq 
  putStrLn ("B:" ++ " (A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC ++ ")")
  putStrLn ("R:" ++ msg)
  mapM_ ((flip countSeqMVar) mcseq) msg 
  (cA, cT, cG, cC) <- readMVar mcseq 
  putStrLn ("M:" ++ " (A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC ++ ")")

main :: IO ()
main = do
  args <- getArgs
  print args
  if length args > 0 && args !! 0 /= "net" then
    mainBenchmark
  else
    if length args > 0 then do
      mcseq <- newMVar emptySeqInfo
      (cA, cT, cG, cC) <- readMVar mcseq 
      putStrLn ("I:" ++ " (A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC ++ ")")
      serveStub "11111" $ countSeqHandler mcseq 
    else
      mainDemo



