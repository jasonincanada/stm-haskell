{-  SleepingBarber.hs

    An implementation of the Sleeping Barber Problem using software transactional memory
    in Haskell with the STM monad. We have two shared memory locations to work with, but
    there are no explicit locks in this code. Instead, we've adopted STM's powerful
    'retry' mechanism for managing concurrent access to shared memory.

    This code is a slight increase in complexity from the previous Cigarette Smokers
    Problem. It demonstrates the compositionality of STM transactions in the 'bring'
    function, which uses 'orElse' to combine the three alternative outcomes of a
    customer's arrival into one composite transaction (see section 3.4 of the PDF linked
    below for more).


 Resources:

    Sleeping Barber Problem
    - https://en.wikipedia.org/wiki/Sleeping_barber_problem

    Composable Memory Transactions (PDF)
    - https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf

-}


module SleepingBarber (sleepingBarber) where

import Control.Concurrent.STM (atomically, retry, orElse, STM, TVar,
                               newTVar, readTVar, modifyTVar, writeTVar)
import Control.Concurrent     (forkIO, threadDelay)
import Control.Monad          (forever, replicateM_, unless, when)
import Data.Function          ((&))
import System.Random          (randomRIO)


type Waiting  = TVar Int    -- number of customers sitting in the waiting area
type Sleeping = TVar Bool   -- whether the barber is sleeping


sleepingBarber :: IO ()
sleepingBarber = do

  -- the barbershop opens with no waiting customers and with the barber asleep
  waiting  <- atomically (newTVar 0   )
  sleeping <- atomically (newTVar True)  

  -- start the barber in his own thread
  goBarber   waiting sleeping & forkIO

  -- generate customer traffic from the sidewalk for a while
  goSidewalk waiting sleeping

  -- wait a few hours for the barber to finish any queued cuts and to fall back asleep
  wait (5*60)


goBarber :: Waiting -> Sleeping -> IO ()
goBarber waiting sleeping =

  forever (sleep >> work)

  where

    -- the barber blocks until woken up by some other thread
    sleep :: IO ()
    sleep = do
      putStrLn "Sleeping..."

      atomically $ readTVar sleeping >>= flip when retry


    work :: IO ()
    work = do
      putStrLn "Cutting hair..."

      -- cuts take somewhere between 5 and 45 minutes
      roll 5 45 >>= wait

      -- after the cut, look for a new customer in the waiting area, or fall asleep if none
      fellAsleep <- atomically $ readTVar waiting
                                   >>= \w -> if w > 0
                                             then writeTVar waiting (w-1) >> return False
                                             else writeTVar sleeping True >> return True
 
      unless fellAsleep work



-- the sidewalk regularly brings in a customer, either waking up the barber, sitting the
-- customer down in the waiting area if there's room, or sending them on their way
goSidewalk :: Waiting -> Sleeping -> IO ()
goSidewalk waiting sleeping = do

  -- send 10 total customers, one every 15 minutes
  replicateM_ 10 (wait 15 >> atomically bring >>= report)

  where

    -- this transaction is run when a customer shows up. it's a composition of the three
    -- possible outcomes. the first one from the left that doesn't 'retry' wins the race
    -- and could have its intended changes (if any) committed to actual memory
    bring :: STM String
    bring = wake `orElse` wait `orElse` leave

      where

        -- if the barber is asleep, wake him up
        wake :: STM String
        wake = readTVar sleeping >>= \s -> if not s
                                           then retry
                                           else do writeTVar sleeping False
                                                   return "Woke the barber!"

        -- sit down if there's a free chair
        wait :: STM String
        wait = readTVar waiting  >>= \w -> if w >= chairs
                                           then retry
                                           else do modifyTVar waiting (+1)
                                                   return "Sat down in waiting area"

        leave :: STM String
        leave = return "Lost the customer"

        chairs = 3  -- chairs in the waiting area


    -- echo to screen the string returned from the winning STM sub-transaction
    report :: String -> IO ()
    report = putStrLn


-- get a random number of minutes
roll :: Int -> Int -> IO Int
roll from to = randomRIO (from,to)

-- one hour in the hair-cutting world goes by in one second on our computer
wait :: Int -> IO ()
wait minutes = threadDelay $ 1000000 * (minutes*60) `div` 3600


{-
    Sleeping...
    Woke the barber!
    Cutting hair...
    Sat down in waiting area
    Cutting hair...
    Sat down in waiting area
    Sat down in waiting area
    Sat down in waiting area
    Cutting hair...
    Sat down in waiting area
    Lost the customer
    Cutting hair...
    Sat down in waiting area
    Lost the customer
    Cutting hair...
    Sat down in waiting area
    Cutting hair...
    Cutting hair...
    Cutting hair...
    Sleeping...
-}

