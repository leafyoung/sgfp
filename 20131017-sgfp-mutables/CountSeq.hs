{-# LANGUAGE BangPatterns #-}
  
module CountSeq (
    SeqInfo(..)
  , emptySeqInfo
  , countSeq
  , countNuc
  , countNuc2
  , countNuc2ST
  , countNuc3
  , countNuc4
  , countNuc5
  , countNuc6
  , countSeqIORef
  , countSeqMVar
  , mainDemo
  , mainBenchmark
) where

import System.Environment
import System.Exit
import System.IO

import Data.IORef
import Data.List
import Control.Concurrent.MVar 

import Control.Monad.ST
import Data.STRef

import Control.Monad.State

import Criterion.Main

type SeqInfo = (Int, Int, Int, Int)
emptySeqInfo = (0,0,0,0)

main :: IO ()
main = do
  args <- getArgs
  print args
  if length args > 0 then
    mainBenchmark
  else
    mainDemo

mainBenchmark :: IO ()
mainBenchmark = do
  seq <- readFile "seq.dna"

  defaultMain [
    bgroup "CountSeq" [ bench "foldl'"       $ whnf countNuc seq -- foldl
                      , bench "State Monad" $ whnf countNuc2 seq -- State Monad
                      , bench "ST Monad"    $ whnf countNuc2ST seq -- ST Monad
                      , bench "IORef"       $ whnfIO $ countNuc3 seq =<< newIORef emptySeqInfo -- IORef
                      , bench "MVar"        $ whnfIO $ countNuc4 seq =<< newMVar emptySeqInfo -- MVar
                      , bench "StateT"      $ whnfIO $ countNuc5 seq -- StateT Monad
                      ] ]

mainDemo :: IO ()
mainDemo = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering

  seq <- readFile "seq.dna"

  putStrLn "foldl':"
  let (cA, cT, cG, cC) = countNuc seq
  putStrLn ("A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC)

  putStrLn "State Monad:"
  let (cA, cT, cG, cC) = countNuc2 seq
  putStrLn ("A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC)
  
  putStrLn "ST Monad:"
  let (cA, cT, cG, cC) = countNuc2ST seq
  putStrLn ("A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC)

  putStrLn "IORef:"
  (cA, cT, cG, cC) <- countNuc3 seq =<< newIORef emptySeqInfo 
  putStrLn ("A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC)

  putStrLn "IORef 2:"
  si <- newIORef emptySeqInfo
  countNuc3 seq si
  (cA, cT, cG, cC) <- readIORef si
  putStrLn ("A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC)

  putStrLn "IORef 3 with 'A'"
  countSeqIORef 'A' si
  (cA, cT, cG, cC) <- readIORef si
  putStrLn ("A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC)

  putStrLn "MVar:"
  si <- newMVar =<< readIORef si 
  countNuc4 seq si
  (cA, cT, cG, cC) <- countNuc4 seq =<< newMVar emptySeqInfo
  putStrLn ("A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC)

  putStrLn "MVar with 'T':"
  countSeqMVar 'T' si
  (cA, cT, cG, cC) <- readMVar si
  putStrLn ("A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC)

  putStrLn "StateT:"
  (cA, cT, cG, cC) <- countNuc5 seq
  putStrLn ("A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC)

  putStrLn "StateT IO : Press ENTER to quit, press AaTtGgCc to count"
  countNuc6 emptySeqInfo

  putStrLn "Press ENTER to quit, press AaTtGgCc to count"
  si <- newMVar emptySeqInfo
  let updateAndPrint :: Char -> IO ()
      updateAndPrint n = do countSeqMVar n si
                            (cA, cT, cG, cC) <- readMVar si
                            putStrLn (n : " :(A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC ++ ")")
                            hFlush stdout
      repeat act1 act2 = do
        n <- act1
        if n == '\n' then 
          return ()
        else do
          if n == '\ESC' then do
            act1 >> act1 >> return ()
          else do
            act2 n
          repeat act1 act2

  repeat getChar (updateAndPrint)

countSeq :: Char -> SeqInfo -> SeqInfo
countSeq n w@(!a,!t,!g,!c) = case n of
    'A' -> (a+1,t,g,c)
    'T' -> (a,t+1,g,c)
    'G' -> (a,t,g+1,c)
    'C' -> (a,t,g,c+1)
    'a' -> (a+1,t,g,c)
    't' -> (a,t+1,g,c)
    'g' -> (a,t,g+1,c)
    'c' -> (a,t,g,c+1)
    otherwise -> w

countNuc :: String -> SeqInfo
countNuc seq = 
  foldl' (flip countSeq) emptySeqInfo seq

countNuc2 :: String -> SeqInfo
countNuc2 seq = (cA, cT, cG, cC)
  where (s, (cA, cT, cG, cC)) = runState (calc seq) emptySeqInfo :: (String, (Int, Int, Int, Int))
        calc (x:xs) = do
          (a,t,g,c) <- get
          -- put $ countNuc seq; return ""
          put $ countSeq x (a,t,g,c)
          calc xs
        calc [] = do
          return ""

countNuc2ST :: String -> SeqInfo
countNuc2ST seq = runST $ do
      cseq <- newSTRef emptySeqInfo
      forM_ seq $ (modifySTRef cseq) . countSeq   
      readSTRef cseq                                  

-- modifyIORef :: IORef a -> (a -> a) -> IO ()
countSeqIORef :: Char -> IORef SeqInfo -> IO ()
countSeqIORef n cseq = modifyIORef cseq $ countSeq n 

countNuc3 :: String -> IORef SeqInfo -> IO SeqInfo 
countNuc3 seq cseq = do
  mapM_ (flip countSeqIORef $ cseq) seq
  return =<< readIORef cseq 

-- modifyMVar :: MVar a -> (a -> IO a) -> IO () 
countSeqMVar :: Char -> MVar SeqInfo -> IO ()
countSeqMVar n cseq = takeMVar cseq >>= (putMVar cseq) . (countSeq n)

countNuc4 :: String -> MVar SeqInfo -> IO SeqInfo
countNuc4 seq cseq = do
  mapM_ ((flip countSeqMVar) cseq) seq
  return =<< readMVar cseq

countNuc5 :: String  -> IO SeqInfo  
countNuc5 seq = do
  (_, cseq) <- runStateT (calc seq) emptySeqInfo
  return cseq
  where calc :: String -> StateT SeqInfo IO ()
        calc (x:xs) = do
          w <- get
          put $ countSeq x w
          calc xs
        calc [] = do
          return ()

countNuc6 cseq = runStateT calc cseq
  where calc :: StateT SeqInfo IO ()
        calc = do
          n <- io $ getChar
          if n == '\n' then 
            return ()
          else do
            if n == '\ESC' then do
              io $ getChar >> getChar >> return ()
            else do
              w <- get
              let w1 = countSeq n w 
              put w1
              let (cA, cT, cG, cC) = w1
              io $ when (w1 /= w) $ do
                hPutStr stdout (n : " (A:" ++ show cA ++ " T:" ++ show cT ++ " G:" ++ show cG ++ " C:" ++ show cC ++ ")\n")
                hFlush stdout
              calc
        io = liftIO


