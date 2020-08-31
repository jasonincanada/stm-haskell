{-# Language RecordWildCards #-}

{-  SushiBar2.hs

    An implementation of the Sushi Bar problem using software transactional memory in
    Haskell with the STM monad. This one fixes the logic error in my first attempt,
    SushiBar1. In that one, a full party is realized only when a 6th customer attempts to
    enter the bar and sees all five of them together. It's more correct to consider them a
    party at the time the 5th person sits down, not just when the next customer notices
    them. So we do need another piece of data to signal that there was once a full five
    customers at the bar even though some may have already left. The next customer in line
    first checks this signal before making any further decision.

    Interestingly, this difference comes about because even as a party the customers leave
    individually when their own plates run out, instead of all at the same time, as you
    might expect from a party. If they were to leave all at once, the first solution
    would've been correct by accident, because there would never be a seat available for
    the sixth customer to impinge on whoever was left from the original party of five.
    Since I've modeled the eating times independently and they leave at their own time, we
    need the extra variable to remind new customers to give the remainder of the party
    time to leave.

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


module SushiBar2 (sushiBar) where

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
                    , signal  :: TVar Bool       -- remaining customers were a party
                    , log     :: String -> IO ()
                    }

barCapacity = 5


{- Main thread -}

sushiBar :: IO ()
sushiBar = do

  seats    <- new []        -- the bar opens with nobody sitting at the bar
  lobby    <- new []        -- and nobody waiting in the lobby to sit down
  signal   <- new False     -- this is on when the remaining customers are part
                            -- of one collective party
  log      <- new []
  
  let bar = Bar seats lobby signal (output log)

  -- start logging to screen
  forkIO (printer log)

  -- start a stream of 20 customers, one every 10 minutes
  forM_ [1..20] $ \name -> forkIO (customer name bar)
                             >> wait 10

  wait (2*60)



{- Customer thread -}

customer :: Customer -> Bar -> IO ()
customer name Bar{..} = do

  say "Entering lobby"

  lineUpFor lobby $ do

      -- wait until all customers from a party have left. the last one out
      -- turns off this signal, then we can proceed
      waitWhile signal True

      atomically (sitDown >> checkSignal)


  say "Eating for 15 to 45 minutes..."
  roll 15 45 >>= wait

  say "Leaving"
  atomically (standUp >> checkSignal)

  where
    sitDown = modifyTVar seats (name:)
    standUp = modifyTVar seats (delete name)

    checkSignal :: STM ()
    checkSignal = readTVar seats >>= with
      where
        with s
          | length s == barCapacity = writeTVar signal True
          | length s == 0           = writeTVar signal False
          | otherwise               = return ()

    say :: String -> IO ()
    say message = log $ printf "%d %s" name message



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


waitWhile :: Eq a => TVar a -> a -> IO ()
waitWhile tvar a = atomically $ readTVar tvar >>= \a' -> when (a' == a) retry

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



{-  Output for 50 minutes between each customer:
    
    1 Entering lobby
    1 Eating for 15 to 45 minutes...
    1 Leaving
    2 Entering lobby
    2 Eating for 15 to 45 minutes...
    2 Leaving
    3 Entering lobby
    3 Eating for 15 to 45 minutes...
    3 Leaving
    4 Entering lobby
    4 Eating for 15 to 45 minutes...
    4 Leaving
    5 Entering lobby
    5 Eating for 15 to 45 minutes...
    5 Leaving
    6 Entering lobby
    6 Eating for 15 to 45 minutes...
    6 Leaving
    7 Entering lobby
    7 Eating for 15 to 45 minutes...
    7 Leaving
    8 Entering lobby
    8 Eating for 15 to 45 minutes...
    8 Leaving
    9 Entering lobby
    9 Eating for 15 to 45 minutes...
    9 Leaving
    10 Entering lobby
    10 Eating for 15 to 45 minutes...
    10 Leaving
    11 Entering lobby
    11 Eating for 15 to 45 minutes...
    11 Leaving
    12 Entering lobby
    12 Eating for 15 to 45 minutes...
    12 Leaving
    13 Entering lobby
    13 Eating for 15 to 45 minutes...
    13 Leaving
    14 Entering lobby
    14 Eating for 15 to 45 minutes...
    14 Leaving
    15 Entering lobby
    15 Eating for 15 to 45 minutes...
    15 Leaving
    16 Entering lobby
    16 Eating for 15 to 45 minutes...
    16 Leaving
    17 Entering lobby
    17 Eating for 15 to 45 minutes...
    17 Leaving
    18 Entering lobby
    18 Eating for 15 to 45 minutes...
    18 Leaving
    19 Entering lobby
    19 Eating for 15 to 45 minutes...
    19 Leaving
    20 Entering lobby
    20 Eating for 15 to 45 minutes...
    20 Leaving


    Output for 5 minutes between each customer:

    1 Entering lobby
    1 Eating for 15 to 45 minutes...
    2 Entering lobby
    2 Eating for 15 to 45 minutes...
    3 Entering lobby
    3 Eating for 15 to 45 minutes...
    4 Entering lobby
    4 Eating for 15 to 45 minutes...
    5 Entering lobby
    5 Eating for 15 to 45 minutes...
    2 Leaving
    6 Entering lobby
    4 Leaving
    7 Entering lobby
    3 Leaving
    8 Entering lobby
    9 Entering lobby
    1 Leaving
    10 Entering lobby
    11 Entering lobby
    12 Entering lobby
    5 Leaving
    6 Eating for 15 to 45 minutes...
    7 Eating for 15 to 45 minutes...
    8 Eating for 15 to 45 minutes...
    9 Eating for 15 to 45 minutes...
    10 Eating for 15 to 45 minutes...
    13 Entering lobby
    14 Entering lobby
    15 Entering lobby
    9 Leaving
    8 Leaving
    16 Entering lobby
    17 Entering lobby
    6 Leaving
    18 Entering lobby
    19 Entering lobby
    7 Leaving
    10 Leaving
    11 Eating for 15 to 45 minutes...
    12 Eating for 15 to 45 minutes...
    13 Eating for 15 to 45 minutes...
    14 Eating for 15 to 45 minutes...
    15 Eating for 15 to 45 minutes...
    20 Entering lobby
    12 Leaving
    15 Leaving
    13 Leaving
    11 Leaving
    14 Leaving
    16 Eating for 15 to 45 minutes...
    17 Eating for 15 to 45 minutes...
    18 Eating for 15 to 45 minutes...
    19 Eating for 15 to 45 minutes...
    20 Eating for 15 to 45 minutes...
    20 Leaving
    17 Leaving
    19 Leaving
    18 Leaving
    16 Leaving
    
-}

