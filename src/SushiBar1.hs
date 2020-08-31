{-# Language RecordWildCards #-}

{-  SushiBar1.hs

    An implementation of the Sushi Bar problem using software transactional memory in
    Haskell with the STM monad. This is not quite a correct solution to the problem. I
    thought I could avoid using a separate signal for determining when an arriving
    customer needs to wait, by having them wait until there's nobody left at the bar if
    there was a full bar when they arrived. However, this doesn't quite make sense in a
    specific circumstance. I'll leave it for the reader to determine what the issue is.
    See SushiBar2.hs for the correct solution and explanation of the issue.

    Here's the problem description from [1] p. 183:

      Imagine a sushi bar with 5 seats. If you arrive while there is an empty seat, you
      can take a seat immediately. But if you arrive when all 5 seats are full, that means
      that all of them are dining together, and you will have to wait for the entire party
      to leave before you sit down.


 References:

    [1] Little Book of Semaphores (problem description on page 183)
        https://greenteapress.com/semaphores/LittleBookOfSemaphores.pdf

    [2] Composable Memory Transactions (PDF)
        https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf

-}


module SushiBar1 (sushiBar) where

import Control.Concurrent.STM (atomically, retry, STM, TVar, newTVar, readTVar, modifyTVar, writeTVar)
import Control.Concurrent     (forkIO, threadDelay, ThreadId)
import Control.Monad          (forever, forM_, unless, when)
import Data.List              (delete)
import System.Random          (randomRIO)
import Text.Printf            (printf)


-- customers are identified by a unique number from 1-20
type Customer = Int

data Bar      = Bar { seats   :: TVar [Customer]
                    , lobby   :: Queue
                    , log     :: String -> IO ()
                    }

barCapacity = 5


{- Main thread -}

sushiBar :: IO ()
sushiBar = do

  seats    <- new []        -- the bar opens with nobody sitting at the bar
  lobby    <- new []        -- and nobody waiting in the lobby to sit down
  log      <- new []
  
  let bar = Bar seats lobby (output log)

  -- start logging to screen
  forkIO (printer log)

  -- start a stream of 20 customers, one every 5 minutes
  forM_ [1..20] $ \name -> forkIO (customer name bar)
                             >> wait 5

  wait (2*60)



{- Customer thread -}

customer :: Customer -> Bar -> IO ()
customer name Bar{..} = do

  say "Entering lobby"

  lineUpFor lobby $ do
    full <- checkBar

    when full $ say "Full bar! Have to wait for everyone to leave..." >>
                waitUntilEmpty

    say "Sitting down and eating..."
    sitDown


  -- eat for 15-45 minutes
  roll 15 45 >>= wait

  say "Leaving"
  leave

  where
    checkBar :: IO Bool
    checkBar = atomically $ readTVar seats
                              >>= \s -> return (length s >= barCapacity)

    waitUntilEmpty = atomically $ readTVar   seats >>= \s -> unless (null s) retry
    sitDown        = atomically $ modifyTVar seats (name:)
    leave          = atomically $ modifyTVar seats (delete name)

    say :: String -> IO ()
    say message = log $ printf "%d %s" name message



{- Utilities -}


-- FIFO queue, callers will block until their turn at the front of the line, then
-- execute the task they were waiting to do at the counter, then leave the line

type Queue = TVar [TVar ()]

lineUpFor :: Queue -> IO () -> IO ()
lineUpFor queue task = do

  us <- new ()

  -- add ourselves to the back of the queue
  atomically $ modifyTVar queue (++[us])

  -- wait until we're next in line at the front of the queue
  atomically $ readTVar queue >>= \q -> unless (head q == us) retry

  task

  atomically $ modifyTVar queue tail


new :: a -> IO (TVar a)
new a = atomically (newTVar a)

-- get a random number of minutes
roll :: Int -> Int -> IO Int
roll from to = randomRIO (from,to)

-- one hour in the hair-cutting world goes by in one second on our computer
wait :: Int -> IO ()
wait minutes = threadDelay $ 1000000 * (minutes*60) `div` 3600



{- Logging -}

type Log   = TVar [String]

output :: Log -> String -> IO ()
output log s = atomically $ modifyTVar log ( s: )

-- print the log as we notice additions to it
printer :: Log -> IO ()
printer log = forever print
  where
    print :: IO ()
    print = do
      lines <- atomically $ do
        l <- readTVar log

        case l of
          [] -> retry

          -- clear the log and return what was in it
          _  -> writeTVar log [] >> return (reverse l)

      -- this is the only call to putStrLn in the whole file
      mapM_ putStrLn lines


{-  λ> sushiBar

    1 Entering lobby
    1 Sitting down and eating...
    2 Entering lobby
    2 Sitting down and eating...
    3 Entering lobby
    3 Sitting down and eating...
    4 Entering lobby
    4 Sitting down and eating...
    5 Entering lobby
    5 Sitting down and eating...
    6 Entering lobby
    6 Full bar! Have to wait for everyone to leave...
    7 Entering lobby
    1 Leaving
    2 Leaving
    8 Entering lobby
    3 Leaving
    9 Entering lobby
    10 Entering lobby
    4 Leaving
    11 Entering lobby
    12 Entering lobby
    5 Leaving
    6 Sitting down and eating...
    7 Sitting down and eating...
    8 Sitting down and eating...
    9 Sitting down and eating...
    10 Sitting down and eating...
    11 Full bar! Have to wait for everyone to leave...
    13 Entering lobby
    14 Entering lobby
    15 Entering lobby
    16 Entering lobby
    9 Leaving
    17 Entering lobby
    18 Entering lobby
    6 Leaving
    7 Leaving
    19 Entering lobby
    20 Entering lobby
    8 Leaving
    10 Leaving
    11 Sitting down and eating...
    12 Sitting down and eating...
    13 Sitting down and eating...
    14 Sitting down and eating...
    15 Sitting down and eating...
    16 Full bar! Have to wait for everyone to leave...
    15 Leaving
    11 Leaving
    14 Leaving
    13 Leaving
    12 Leaving
    16 Sitting down and eating...
    17 Sitting down and eating...
    18 Sitting down and eating...
    19 Sitting down and eating...
    20 Sitting down and eating...
    16 Leaving
    18 Leaving
    20 Leaving
    17 Leaving
    19 Leaving
-}

