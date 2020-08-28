{-# Language LambdaCase, RecordWildCards #-}

{-  MultipleBarbers.hs

    Checking in first draft that sort of works... still some bugs to iron out and it
    could do with some more refactoring

    Bugs:
      - only 1 barber seems to fall asleep at the end of the output, the other 2 are
        still in TakingPayment
      - commonly seeing 5 satisfied customer messages in a row but there should be max 3



 References:

    [1] Little Book of Semaphores (problem description on page 133)
        https://greenteapress.com/semaphores/LittleBookOfSemaphores.pdf

    [2] Composable Memory Transactions (PDF)
        https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf

-}


module MultipleBarbers (multipleBarbers) where

import Control.Concurrent.STM (atomically, retry, orElse, STM, TVar, newTVar, readTVar, modifyTVar, writeTVar)
import Control.Concurrent     (forkIO, threadDelay, ThreadId)
import Control.Monad          (forever, forM_, unless, when)
import System.Random          (randomRIO)


-- the state of a customer is the barber they're paired up with
type MyBarber   = TVar (Maybe (TVar Barber))
type Customer   = MyBarber

data Barber     = Sleeping | Cutting | TakingPayment
                  deriving (Eq, Show)

data BarberShop = BarberShop {
                    barber1  :: TVar Barber,
                    barber2  :: TVar Barber,
                    barber3  :: TVar Barber,
                    couch    :: TVar [Customer],
                    standing :: TVar [Customer],
                    cash     :: TVar Bool,      -- is the cash register in use
                    payment  :: TVar Bool,      -- customer placed money on the counter
                    log      :: String -> IO ()
                  }

couchCapacity = 4
customerLimit = 15


-- alias the orElse operator to something more terse and fancy
(|>) :: STM a -> STM a -> STM a
(|>) = orElse



{- Main thread -}

multipleBarbers :: IO ()
multipleBarbers = do

  couch    <- new []        -- the shop opens with nobody sitting on the couch
  standing <- new []        -- nobody standing in the waiting area 
  cash     <- new False     -- nobody operating the cash register
  payment  <- new False     -- no payment on the counter
  log      <- new []        -- nothing narrated yet
  
  barber1  <- new Sleeping  -- and all three barbers sleeping in their chairs
  barber2  <- new Sleeping
  barber3  <- new Sleeping

  -- collect our tvars all in one place so we can share them easily with the threads
  let shop = BarberShop barber1 barber2 barber3
                        couch standing
                        cash payment
                        (output log)

  forkIO $ barber 1 barber1 shop
  forkIO $ barber 2 barber2 shop
  forkIO $ barber 3 barber3 shop

  forkIO $ goPrinter log

  -- start a stream of 20 customers, one every 5 minutes
  forM_ [1..20] (\name -> wait 5 >> show name `arriveAt` shop)

  -- confirm 0 people standing and sitting after two hours
  wait (2*60)

  atomically (readTVar couch   ) >>= print . length
  atomically (readTVar standing) >>= print . length
  atomically (readTVar barber1 ) >>= print
  atomically (readTVar barber2 ) >>= print
  atomically (readTVar barber3 ) >>= print

  where
    -- start a thread for a new customer arriving at the shop
    arriveAt :: String -> BarberShop -> IO ThreadId
    arriveAt name shop = do cust  <- new Nothing
                            forkIO $ customer name cust shop


{- Barber thread -}

barber :: Int -> TVar Barber -> BarberShop -> IO ()
barber i im BarberShop{..} =
  
  forever (sleep >> work)

  where
    sleep :: IO ()
    sleep = do
      log $ "Barber " ++ show i ++ " sleeping..."

      waitWhile im Sleeping


    work :: IO ()
    work = do
      log $ "Barber " ++ show i ++ " cutting hair..."

      -- cuts take somewhere between 15 and 45 minutes
      roll 15 45 >>= wait

      -- after the cut, wait for our turn to use the cash register
      waitUntil cash False

      -- the barber is ready for a payment (the customer watches for this change)
      now im TakingPayment

      -- wait for the customer to place his payment on the counter, then pick it up
      waitUntil payment True
      now payment False

      log $ "Barber " ++ show i ++ " done with customer"

      fellAsleep <- nextCustomer
      unless fellAsleep work

      where

        -- good mix here of sequential and alternative composition (see [2] sec. 3)

        -- take the next customer from the couch or fall back asleep if it's empty
        nextCustomer :: IO Bool
        nextCustomer = atomically (tryCouch |> fallAsleep)

        tryCouch :: STM Bool
        tryCouch = standUp >> sitDown >> return False

        -- if anyone's on the couch, pair them with this barber
        standUp :: STM ()
        standUp = readTVar couch
                    >>= \case [] -> retry
                              cs -> do writeTVar couch (tail cs)
                                       writeTVar (head cs) (Just im)

        -- if anyone's standing in the waiting area they can now sit down
        sitDown :: STM ()
        sitDown = readTVar standing
                    >>= \case [] -> return ()
                              ss -> do modifyTVar couch (++[last ss])
                                       modifyTVar standing init


        -- the alternative transaction if tryCouch retrys
        fallAsleep :: STM Bool
        fallAsleep = writeTVar im Sleeping >> return True



{- Customer thread -}

-- the four possible outcomes of a customer trying to enter the barbershop
data Result = Woke | SatDown | Standing | Lost


customer :: String -> MyBarber -> BarberShop -> IO ()
customer name myBarber BarberShop{..} = do

  thus  <- atomically $ wake barber1 |>
                        wake barber2 |>
                        wake barber3 |>
                        sit          |>
                        stand        |>
                        leave

  case thus of
    Woke     -> log (name ++ " Woke a barber") >> waitFor myBarber >>= getHairCut
    SatDown  -> log (name ++ " Sat down...")   >> waitFor myBarber >>= getHairCut
    Standing -> log (name ++ " Standing...")   >> waitFor myBarber >>= getHairCut
    Lost     -> log (name ++ " Lost customer")

  where

    wake :: TVar Barber -> STM Result
    wake b = readTVar b >>= \case Sleeping  -> writeTVar myBarber (Just b) >>
                                               writeTVar b Cutting         >>
                                               return Woke

                                  _         -> retry

    sit, stand, leave :: STM Result
    sit    = readTVar couch    >>= \c -> if length c >= couchCapacity
                                         then retry
                                         else modifyTVar couch (++[myBarber]) >>
                                              return SatDown

    stand  = readTVar standing >>= \s -> if length s + couchCapacity + 3 >= customerLimit
                                         then retry
                                         else modifyTVar standing (++[myBarber]) >>
                                              return Standing

    leave  = return Lost


    getHairCut :: TVar Barber -> IO ()
    getHairCut barber = do

      -- consider our cut done when the barber has reached the cash register for payment
      waitUntil barber TakingPayment

      log (name ++ " Paying...")
      now payment True           -- place our payment on the counter
      waitUntil payment False    -- wait for the barber to pick it up

      -- cut done!
      log (name ++ " Another satisfied customer!")



{- Utilities -}

-- some words for our concurrency DSL
waitUntil :: Eq a => TVar a -> a -> IO ()
waitUntil tvar a = atomically $ readTVar tvar >>= \a' -> unless (a' == a) retry
waitWhile tvar a = atomically $ readTVar tvar >>= \a' -> when   (a' == a) retry

-- wait for an optional value until there's one there
waitFor :: TVar (Maybe a) -> IO a
waitFor tvar = atomically $ readTVar tvar >>= \case Nothing -> retry
                                                    Just a  -> return a

now :: TVar a -> a -> IO ()
now tvar a = atomically (writeTVar tvar a)

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


{-  λ> multipleBarbers

    Barber 1 sleeping...
    Barber 2 sleeping...
    Barber 3 sleeping...
    1 Woke a barber
    Barber 1 cutting hair...
    2 Woke a barber
    Barber 2 cutting hair...
    3 Woke a barber
    Barber 3 cutting hair...
    4 Sat down...
    5 Sat down...
    2 Paying...
    Barber 2 done with customer
    Barber 2 cutting hair...
    2 Another satisfied customer!
    4 Paying...
    6 Sat down...
    Barber 3 done with customer
    3 Paying...
    Barber 3 cutting hair...
    5 Paying...
    Barber 1 done with customer
    Barber 1 cutting hair...
    1 Paying...
    6 Paying...
    7 Sat down...
    8 Sat down...
    Barber 2 done with customer
    Barber 2 cutting hair...
    4 Another satisfied customer!
    3 Another satisfied customer!
    5 Another satisfied customer!
    1 Another satisfied customer!
    6 Another satisfied customer!
    7 Paying...
    9 Sat down...
    10 Sat down...
    11 Sat down...
    Barber 1 done with customer
    7 Another satisfied customer!
    Barber 1 cutting hair...
    8 Paying...
    12 Sat down...
    Barber 2 done with customer
    Barber 2 cutting hair...
    8 Another satisfied customer!
    9 Paying...
    13 Sat down...
    14 Standing...
    Barber 3 done with customer
    Barber 3 cutting hair...
    9 Another satisfied customer!
    10 Paying...
    15 Standing...
    16 Standing...
    17 Standing...
    Barber 3 done with customer
    Barber 3 cutting hair...
    10 Another satisfied customer!
    11 Paying...
    18 Standing...
    Barber 2 done with customer
    Barber 2 cutting hair...
    11 Another satisfied customer!
    12 Paying...
    19 Standing...
    20 Standing...
    Barber 3 done with customer
    Barber 3 cutting hair...
    12 Another satisfied customer!
    13 Paying...
    Barber 1 done with customer
    Barber 1 cutting hair...
    13 Another satisfied customer!
    14 Paying...
    Barber 1 done with customer
    14 Another satisfied customer!
    Barber 1 cutting hair...
    17 Paying...
    Barber 3 done with customer
    Barber 3 cutting hair...
    17 Another satisfied customer!
    18 Paying...
    Barber 2 done with customer
    Barber 2 cutting hair...
    18 Another satisfied customer!
    20 Paying...
    Barber 1 done with customer
    Barber 1 cutting hair...
    20 Another satisfied customer!
    19 Paying...
    Barber 2 done with customer
    Barber 2 cutting hair...
    19 Another satisfied customer!
    16 Paying...
    Barber 3 done with customer
    Barber 3 cutting hair...
    16 Another satisfied customer!
    15 Paying...
    Barber 2 done with customer
    Barber 2 sleeping...
    15 Another satisfied customer!
    0
    0
    TakingPayment
    Sleeping
    TakingPayment
-}

