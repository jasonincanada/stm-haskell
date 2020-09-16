
{-  SantaClaus.hs

    An implementation of the Santa Claus concurrency problem in Haskell using software
    transactional memory. The challenge is to coordinate the actions of multiple
    threads so they start at the same time. The cycle of actions that the reindeer and
    elves go through are essentially the same, so we have one generic function goActor
    that operates both types.

    The main pitfall to be aware of with this problem is queue-jumping, in particular
    other elves jumping in front of the 3 at the front of the queue once Santa has started
    collecting them. But we don't have to worry about this because the crew is always
    atomically pulled from the front of the queue all at once, and then referenced locally
    by the Santa thread. So any actors lining up behind the ones just selected have no
    choice but to wait until Santa is ready to check the queues again.


 Resources:
    
    The Santa Claus Problem - Thread Synchronization
    - https://www.youtube.com/watch?v=pqO6tKN2lc4

    Composable Memory Transactions (PDF)
    - https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf

-}


module SantaClaus (santaClaus) where

import Control.Concurrent.STM (atomically, retry, orElse, STM, TVar, newTVar, readTVar, modifyTVar, writeTVar)
import Control.Concurrent     (forkIO, threadDelay)
import Control.Monad          (forever, forM_, unless, when)
import System.Random          (randomRIO)
import Text.Printf            (printf)


data Status = Away
            | Hitched      -- hitched to sleigh or ushered into santa's office
            | WithSanta    -- working on our task with santa
            | Waiting      -- done and waiting to be ushered away
            | Unhitched    -- free to go
              deriving (Eq)

type Actor = TVar (Int, Status)
type Queue = TVar [Actor]


santaClaus :: IO ()
santaClaus = do

  -- reindeer line up in the stable while waiting, and elves in the lobby
  stable  <- atomically (newTVar [])
  lobby   <- atomically (newTVar [])
  log     <- atomically (newTVar [])

  -- start the reindeer and elf threads
  forM_ [1..9 ] $ \name -> forkIO $ goActor "🦌 Reindeer"
                                            name
                                            "deliver toys"
                                            stable
                                            log

  forM_ [1..10] $ \name -> forkIO $ goActor "Elf"
                                            name
                                            "consult with Santa"
                                            lobby
                                            log

  -- start santa in his own thread
  forkIO $ goSanta stable lobby log

  -- course all output through a single putStrLn to avoid jumbling simultaneous messages
  forkIO $ goPrinter log

  -- watch activity at the north pole for a few hours
  wait (3*60)


goActor :: String -> Int -> String -> Queue -> Log -> IO ()
goActor actorType name need queue log = do
  
  say "Starting thread"

  -- create a memory location for our name and status and start off Away
  we  <- atomically $ newTVar (name, Away)

  forever $ do

    -- we won't need santa's help for a while
    roll 1 60 >>= wait

    say ("Need to " ++ need)  >> we `waitIn`    queue
    say "Being gathered"      >> we `waitUntil` WithSanta
    say "Working with Santa!"

    -- toy delivery / consultation by the actors is instant for this simple example
    -- but actual IO work can occur here

    -- wait to be unhitched from the sleigh or dismissed from the office
    we `now`       Waiting
    we `waitUntil` Unhitched

    say "Away we go, thanks Santa!"
    we `now`       Away


  where

    waitIn :: Actor -> Queue -> IO ()
    waitIn tvar queue = do

      -- add ourselves to the back of the queue
      atomically $ modifyTVar queue (++[tvar])

      -- wait until we're no longer in the queue, meaning santa removed us
      atomically $ readTVar queue >>= \q -> when (tvar `elem` q) retry


    waitUntil :: Actor -> Status -> IO ()
    waitUntil actor s = atomically $ readTVar actor
                                       >>= \(_, status) -> unless (s == status) retry
                    
    now :: Actor -> Status -> IO ()
    now actor s = atomically $ modifyTVar actor $ fmap (const s)

    say :: String -> IO ()
    say message = output log $ printf "%s %-2d- %s" actorType name message



goSanta :: Queue -> Queue -> Log -> IO ()
goSanta stable lobby log =

  forever $ do

    (actors, task, doing) <- atomically (getReindeer `orElse` getElves)

    say $ "Waking up for " ++ doing

    -- prepare all the actors
    forM_ actors $ \actor -> do
      (name, status) <- getA actor

      say $ "Preparing " ++ show name
      modifyA actor $ fmap (const Hitched)

    -- it takes a minute to walk to the sleigh or sit down in the office
    wait 1

    say $ "Now " ++ doing
    setAll actors WithSanta

    task

    -- wait for all actors to be back Waiting to be unhitched or sent away
    forM_ actors $ \actor -> 
      atomically $ readTVar actor >>= \(_, s) -> unless (s == Waiting) retry

    say "Releasing the crew..."
    setAll actors Unhitched


  where

    getReindeer, getElves :: STM ([Actor], IO (), String)
    getReindeer = needsAtLeast stable 9 deliverToys  "delivering toys!"
    getElves    = needsAtLeast lobby  3 consultSanta "consulting with elves"


    -- wait until a queue has a certain number of actors waiting, then remove
    -- them from the queue and return them to the calling code
    --
    -- note we cannot actually execute IO actions within an STM transaction, but
    -- nothing stops us from returning an IO action *from* one!
    needsAtLeast :: Queue -> Int -> IO () -> String -> STM ([Actor], IO (), String)
    needsAtLeast queue n task doing =

      readTVar queue >>= \q -> if length q < n
                               then retry
                               else do tvars <- take n <$> readTVar queue
                                       modifyTVar queue $ drop n
                                       return (tvars, task, doing)


    -- set all actors to a status, one IO action at a time (not all at once), to emulate
    -- santa walking around hitching up reindeer or ushering elves into his office
    setAll :: [Actor] -> Status -> IO ()
    setAll actors s = forM_ actors $ \actor ->
                        modifyA actor $ fmap (const s)

    say :: String -> IO ()
    say message = output log $ printf "Santa 🎅 - %s" message


deliverToys :: IO ()
deliverToys = wait 10

consultSanta :: IO ()
consultSanta = wait 10



{- Utilities -}

-- atomically get or modify a TVar
getA :: TVar a -> IO a
getA tvar = atomically (readTVar tvar)

modifyA :: TVar a -> (a -> a) -> IO ()
modifyA tvar f = atomically (modifyTVar tvar f)

-- get a random number of minutes
roll :: Int -> Int -> IO Int
roll from to = randomRIO (from,to)

-- one hour at the north pole goes by in one second on our computer
wait :: Int -> IO ()
wait minutes = threadDelay $ 1000000 * (minutes*60) `div` 3600


{- Logging -}

type Log = TVar [String]

goPrinter :: Log -> IO ()
goPrinter log = forever print
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


-- called from within threads that have been passed a Log
output :: Log -> String -> IO ()
output log s = atomically $ modifyTVar log ( s: )


{-
    🦌 Reindeer 1 - Starting thread
    🦌 Reindeer 2 - Starting thread
    🦌 Reindeer 3 - Starting thread
    🦌 Reindeer 4 - Starting thread
    🦌 Reindeer 5 - Starting thread
    🦌 Reindeer 6 - Starting thread
    🦌 Reindeer 7 - Starting thread
    🦌 Reindeer 8 - Starting thread
    🦌 Reindeer 9 - Starting thread
    Elf 1 - Starting thread
    Elf 2 - Starting thread
    Elf 3 - Starting thread
    Elf 4 - Starting thread
    Elf 5 - Starting thread
    Elf 6 - Starting thread
    Elf 7 - Starting thread
    Elf 8 - Starting thread
    Elf 9 - Starting thread
    Elf 10- Starting thread

    🦌 Reindeer 6 - Need to deliver toys
    Elf 1 - Need to consult with Santa
    Elf 6 - Need to consult with Santa
    🦌 Reindeer 7 - Need to deliver toys
    🦌 Reindeer 8 - Need to deliver toys
    🦌 Reindeer 9 - Need to deliver toys
    🦌 Reindeer 5 - Need to deliver toys
    Elf 7 - Need to consult with Santa
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 1
    Santa 🎅 - Preparing 6
    Santa 🎅 - Preparing 7
    Elf 7 - Being gathered
    Elf 6 - Being gathered
    Elf 1 - Being gathered
    Santa 🎅 - Now consulting with elves
    Elf 1 - Working with Santa!
    Elf 6 - Working with Santa!
    Elf 7 - Working with Santa!
    Elf 4 - Need to consult with Santa
    🦌 Reindeer 3 - Need to deliver toys
    Elf 5 - Need to consult with Santa
    Santa 🎅 - Releasing the crew...
    Elf 1 - Away we go, thanks Santa!
    Elf 6 - Away we go, thanks Santa!
    Elf 7 - Away we go, thanks Santa!
    Elf 2 - Need to consult with Santa
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 4
    Santa 🎅 - Preparing 5
    Santa 🎅 - Preparing 2
    Elf 2 - Being gathered
    Elf 5 - Being gathered
    Elf 4 - Being gathered
    Santa 🎅 - Now consulting with elves
    Elf 4 - Working with Santa!
    Elf 5 - Working with Santa!
    Elf 2 - Working with Santa!
    Elf 6 - Need to consult with Santa
    Elf 3 - Need to consult with Santa
    Elf 8 - Need to consult with Santa
    Santa 🎅 - Releasing the crew...
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 6
    Santa 🎅 - Preparing 3
    Santa 🎅 - Preparing 8
    Elf 4 - Away we go, thanks Santa!
    Elf 5 - Away we go, thanks Santa!
    Elf 2 - Away we go, thanks Santa!
    Elf 8 - Being gathered
    Elf 3 - Being gathered
    Elf 6 - Being gathered
    Santa 🎅 - Now consulting with elves
    Elf 6 - Working with Santa!
    Elf 3 - Working with Santa!
    Elf 8 - Working with Santa!
    Elf 10- Need to consult with Santa
    🦌 Reindeer 1 - Need to deliver toys
    🦌 Reindeer 2 - Need to deliver toys
    Elf 2 - Need to consult with Santa
    🦌 Reindeer 4 - Need to deliver toys
    Elf 9 - Need to consult with Santa
    Elf 7 - Need to consult with Santa
    Santa 🎅 - Releasing the crew...
    Santa 🎅 - Waking up for delivering toys!
    Santa 🎅 - Preparing 6
    Santa 🎅 - Preparing 7
    Santa 🎅 - Preparing 8
    Santa 🎅 - Preparing 9
    Santa 🎅 - Preparing 5
    Santa 🎅 - Preparing 3
    Santa 🎅 - Preparing 1
    Santa 🎅 - Preparing 2
    Santa 🎅 - Preparing 4
    Elf 6 - Away we go, thanks Santa!
    Elf 3 - Away we go, thanks Santa!
    Elf 8 - Away we go, thanks Santa!
    🦌 Reindeer 4 - Being gathered
    🦌 Reindeer 2 - Being gathered
    🦌 Reindeer 1 - Being gathered
    🦌 Reindeer 5 - Being gathered
    🦌 Reindeer 9 - Being gathered
    🦌 Reindeer 8 - Being gathered
    🦌 Reindeer 6 - Being gathered
    🦌 Reindeer 7 - Being gathered
    🦌 Reindeer 3 - Being gathered
    Santa 🎅 - Now delivering toys!
    🦌 Reindeer 6 - Working with Santa!
    🦌 Reindeer 7 - Working with Santa!
    🦌 Reindeer 8 - Working with Santa!
    🦌 Reindeer 9 - Working with Santa!
    🦌 Reindeer 5 - Working with Santa!
    🦌 Reindeer 3 - Working with Santa!
    🦌 Reindeer 1 - Working with Santa!
    🦌 Reindeer 2 - Working with Santa!
    🦌 Reindeer 4 - Working with Santa!
    Elf 8 - Need to consult with Santa
    Elf 3 - Need to consult with Santa
    Santa 🎅 - Releasing the crew...
    🦌 Reindeer 6 - Away we go, thanks Santa!
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 10
    Santa 🎅 - Preparing 2
    Santa 🎅 - Preparing 9
    🦌 Reindeer 7 - Away we go, thanks Santa!
    🦌 Reindeer 8 - Away we go, thanks Santa!
    🦌 Reindeer 9 - Away we go, thanks Santa!
    🦌 Reindeer 5 - Away we go, thanks Santa!
    🦌 Reindeer 3 - Away we go, thanks Santa!
    🦌 Reindeer 1 - Away we go, thanks Santa!
    🦌 Reindeer 2 - Away we go, thanks Santa!
    🦌 Reindeer 4 - Away we go, thanks Santa!
    Elf 9 - Being gathered
    Elf 2 - Being gathered
    Elf 10- Being gathered
    Santa 🎅 - Now consulting with elves
    🦌 Reindeer 9 - Need to deliver toys
    🦌 Reindeer 1 - Need to deliver toys
    Elf 10- Working with Santa!
    Elf 2 - Working with Santa!
    Elf 9 - Working with Santa!
    🦌 Reindeer 6 - Need to deliver toys
    🦌 Reindeer 3 - Need to deliver toys
    Elf 5 - Need to consult with Santa
    🦌 Reindeer 2 - Need to deliver toys
    Santa 🎅 - Releasing the crew...
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 7
    Santa 🎅 - Preparing 8
    Santa 🎅 - Preparing 3
    Elf 10- Away we go, thanks Santa!
    Elf 2 - Away we go, thanks Santa!
    Elf 9 - Away we go, thanks Santa!
    Elf 3 - Being gathered
    Elf 8 - Being gathered
    Elf 7 - Being gathered
    Santa 🎅 - Now consulting with elves
    Elf 7 - Working with Santa!
    Elf 8 - Working with Santa!
    Elf 3 - Working with Santa!
    🦌 Reindeer 4 - Need to deliver toys
    Elf 6 - Need to consult with Santa
    🦌 Reindeer 7 - Need to deliver toys
    Santa 🎅 - Releasing the crew...
    Elf 7 - Away we go, thanks Santa!
    Elf 8 - Away we go, thanks Santa!
    Elf 3 - Away we go, thanks Santa!
    Elf 1 - Need to consult with Santa
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 5
    Santa 🎅 - Preparing 6
    Santa 🎅 - Preparing 1
    Elf 1 - Being gathered
    Elf 6 - Being gathered
    Elf 5 - Being gathered
    Elf 3 - Need to consult with Santa
    Santa 🎅 - Now consulting with elves
    Elf 5 - Working with Santa!
    Elf 6 - Working with Santa!
    Elf 1 - Working with Santa!
    Elf 4 - Need to consult with Santa
    Elf 8 - Need to consult with Santa
    Elf 10- Need to consult with Santa
    Santa 🎅 - Releasing the crew...
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 3
    Santa 🎅 - Preparing 4
    Santa 🎅 - Preparing 8
    Elf 5 - Away we go, thanks Santa!
    Elf 6 - Away we go, thanks Santa!
    Elf 1 - Away we go, thanks Santa!
    Elf 8 - Being gathered
    Elf 4 - Being gathered
    Elf 3 - Being gathered
    Santa 🎅 - Now consulting with elves
    Elf 3 - Working with Santa!
    Elf 4 - Working with Santa!
    Elf 8 - Working with Santa!
    Elf 9 - Need to consult with Santa
    🦌 Reindeer 8 - Need to deliver toys
    Santa 🎅 - Releasing the crew...
    Elf 3 - Away we go, thanks Santa!
    Elf 4 - Away we go, thanks Santa!
    Elf 8 - Away we go, thanks Santa!
    🦌 Reindeer 5 - Need to deliver toys
    Santa 🎅 - Waking up for delivering toys!
    Santa 🎅 - Preparing 9
    Santa 🎅 - Preparing 1
    Santa 🎅 - Preparing 6
    Santa 🎅 - Preparing 3
    Santa 🎅 - Preparing 2
    Santa 🎅 - Preparing 4
    Santa 🎅 - Preparing 7
    Santa 🎅 - Preparing 8
    Santa 🎅 - Preparing 5
    🦌 Reindeer 5 - Being gathered
    🦌 Reindeer 8 - Being gathered
    🦌 Reindeer 7 - Being gathered
    🦌 Reindeer 4 - Being gathered
    🦌 Reindeer 2 - Being gathered
    🦌 Reindeer 3 - Being gathered
    🦌 Reindeer 6 - Being gathered
    🦌 Reindeer 1 - Being gathered
    🦌 Reindeer 9 - Being gathered
    Santa 🎅 - Now delivering toys!
    🦌 Reindeer 9 - Working with Santa!
    🦌 Reindeer 1 - Working with Santa!
    🦌 Reindeer 6 - Working with Santa!
    🦌 Reindeer 3 - Working with Santa!
    🦌 Reindeer 2 - Working with Santa!
    🦌 Reindeer 4 - Working with Santa!
    🦌 Reindeer 7 - Working with Santa!
    🦌 Reindeer 8 - Working with Santa!
    🦌 Reindeer 5 - Working with Santa!
    Elf 1 - Need to consult with Santa
    Santa 🎅 - Releasing the crew...
    🦌 Reindeer 9 - Away we go, thanks Santa!
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 10
    Santa 🎅 - Preparing 9
    Santa 🎅 - Preparing 1
    🦌 Reindeer 1 - Away we go, thanks Santa!
    🦌 Reindeer 6 - Away we go, thanks Santa!
    🦌 Reindeer 3 - Away we go, thanks Santa!
    🦌 Reindeer 2 - Away we go, thanks Santa!
    🦌 Reindeer 4 - Away we go, thanks Santa!
    🦌 Reindeer 7 - Away we go, thanks Santa!
    🦌 Reindeer 8 - Away we go, thanks Santa!
    🦌 Reindeer 5 - Away we go, thanks Santa!
    Elf 1 - Being gathered
    Elf 9 - Being gathered
    Elf 10- Being gathered
    Santa 🎅 - Now consulting with elves
    Elf 10- Working with Santa!
    Elf 9 - Working with Santa!
    Elf 1 - Working with Santa!
    Elf 2 - Need to consult with Santa
    🦌 Reindeer 3 - Need to deliver toys
    Santa 🎅 - Releasing the crew...
    Elf 10- Away we go, thanks Santa!
    Elf 9 - Away we go, thanks Santa!
    Elf 1 - Away we go, thanks Santa!
    Elf 6 - Need to consult with Santa
    Elf 3 - Need to consult with Santa
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 2
    Santa 🎅 - Preparing 6
    Santa 🎅 - Preparing 3
    Elf 3 - Being gathered
    Elf 6 - Being gathered
    Elf 2 - Being gathered
    Santa 🎅 - Now consulting with elves
    Elf 2 - Working with Santa!
    Elf 6 - Working with Santa!
    Elf 3 - Working with Santa!
    Elf 7 - Need to consult with Santa
    Elf 4 - Need to consult with Santa
    🦌 Reindeer 8 - Need to deliver toys
    Santa 🎅 - Releasing the crew...
    Elf 2 - Away we go, thanks Santa!
    Elf 6 - Away we go, thanks Santa!
    Elf 3 - Away we go, thanks Santa!
    Elf 10- Need to consult with Santa
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 7
    Santa 🎅 - Preparing 4
    Santa 🎅 - Preparing 10
    Elf 10- Being gathered
    Elf 4 - Being gathered
    Elf 7 - Being gathered
    Elf 5 - Need to consult with Santa
    Elf 3 - Need to consult with Santa
    Santa 🎅 - Now consulting with elves
    Elf 7 - Working with Santa!
    Elf 4 - Working with Santa!
    Elf 10- Working with Santa!
    🦌 Reindeer 4 - Need to deliver toys
    🦌 Reindeer 2 - Need to deliver toys
    Santa 🎅 - Releasing the crew...
    Elf 7 - Away we go, thanks Santa!
    Elf 4 - Away we go, thanks Santa!
    Elf 10- Away we go, thanks Santa!
    🦌 Reindeer 6 - Need to deliver toys
    Elf 8 - Need to consult with Santa
    Santa 🎅 - Waking up for consulting with elves
    Santa 🎅 - Preparing 5
    Santa 🎅 - Preparing 3
    Santa 🎅 - Preparing 8
    Elf 8 - Being gathered
    Elf 3 - Being gathered
    Elf 5 - Being gathered
    Santa 🎅 - Now consulting with elves
    Elf 5 - Working with Santa!
    Elf 3 - Working with Santa!
    Elf 8 - Working with Santa!

-}

