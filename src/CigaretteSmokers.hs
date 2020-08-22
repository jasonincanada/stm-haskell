{-# Language LambdaCase #-}

{-  CigaretteSmokers.hs

    An implementation of the Cigarette Smokers Problem using software transactional memory
    in Haskell with the STM monad. Although we use shared memory locations, there are no
    explicit locks or mutexes in this code--we have thrown them to the wind and adopted
    STM's powerful 'retry' mechanism instead.


 Resources:

    Cigarette Smokers Problem
    - https://en.wikipedia.org/wiki/Cigarette_smokers_problem

    Composable Memory Transactions (PDF)
    - https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf

-}


module CigaretteSmokers (cigaretteSmokers) where

import Control.Concurrent.STM (atomically, newTVar, readTVar, writeTVar, retry, STM, TVar)
import Control.Concurrent     (forkIO, threadDelay, ThreadId)
import Control.Monad          (forever, forM_)
import System.Random          (randomRIO)


-- indicate whether a material is on the table or not with a transactional variable
type Material = TVar Bool


cigaretteSmokers :: IO ()
cigaretteSmokers = do

  -- set up places for the three different ingredients, which haven't yet been placed
  tobacco <- atomically (newTVar False)
  paper   <- atomically (newTVar False)
  matches <- atomically (newTVar False)

  -- we'll use only one semaphore for any smoker to signal that they're done smoking
  signal  <- atomically (newTVar False)

  -- start our smoker threads with pointers to the ingredients they need and the
  -- shared signalling semaphore
  smoker "Tobacco" paper   matches signal
  smoker "Paper"   tobacco matches signal
  smoker "Matches" tobacco paper   signal


  forM_ [1..5] $ \i -> do

    -- place two random ingredients on the table
    roll >>= \case 1 -> place paper   >> place matches
                   2 -> place tobacco >> place matches
                   3 -> place tobacco >> place paper

    -- wait for the appropriate smoker to finish their smoke
    waitOnSmoker signal


waitOnSmoker :: TVar Bool -> IO ()
waitOnSmoker signal = wait >> reset
  where 
    -- wait for the smoker to set the done signal to True
    wait  = atomically $ readTVar signal >>= \case False -> retry
                                                   True  -> return ()

    reset = remove signal

              
place, remove :: Material -> IO ()
place  m = atomically (writeTVar m True )
remove m = atomically (writeTVar m False)


-- a smoker waits until the two ingredients they need are placed on the table, then takes
-- them, smokes for 1 second, then signals to the agent to continue
smoker :: String -> Material -> Material -> TVar Bool -> IO ThreadId
smoker name first second signal = do

  putStrLn $ "Starting thread for " ++ name

  forkIO $ forever (atomically gather >> smoke >> place signal)

  where

    gather :: STM ()
    gather = do
    
      -- try to get both items we need from the table. all smokers are doing this at the
      -- same time while waiting for the agent to place ingredients down. the first smoker
      -- to see both required ingredients wins the race
      ingredient1 <- readTVar first
      ingredient2 <- readTVar second

      if ingredient1 && ingredient2

        then do

          -- pick up the ingredients, making them no longer on the table
          writeTVar first  False
          writeTVar second False

        else

          -- such a simple call, retry, but it's smart enough to wait until at least one
          -- of the readTVar variables above has changed before actually retrying the
          -- transaction. otherwise it will obviously end up right back at this retry!
          retry


    smoke :: IO ()
    smoke = putStrLn (name ++ " smokes for 1 second...")
              >> wait 1


-- roll for a number between 1 and 3
roll :: IO Int
roll = randomRIO (1,3)

wait :: Int -> IO ()
wait sec = threadDelay (sec * 1000000)


{-
    λ> cigaretteSmokers

    Starting thread for Tobacco
    Starting thread for Paper
    Starting thread for Matches
    Paper smokes for 1 second...
    Matches smokes for 1 second...
    Paper smokes for 1 second...
    Tobacco smokes for 1 second...
    Tobacco smokes for 1 second...
-}

