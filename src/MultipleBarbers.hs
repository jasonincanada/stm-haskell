{-# Language LambdaCase, RecordWildCards #-}

{-  MultipleBarbers.hs

    An implementation of the Multiple Barbers problem using software transactional memory
    in Haskell with the STM monad. It models the orderly coordination of customers and
    barbers in a 3-chair barbershop, with room for 4 customers to wait on a couch and room
    for 8 additional customers to stand while waiting.

    After a barber finishes a cut and cashes out their customer, they first check the
    couch for anyone waiting, taking the longest-waiting customer if so, or they fall
    asleep in their chair if there's no customer. When a customer is drawn from the couch,
    any standing customer sits down, in order of who has been standing the longest.

    This is tricky to implement using traditional lock-based mechanics (see [1] p. 135)
    but pretty easy with new transactional memory techniques [2], in particular the retry
    mechanism, which is used wherever you would see explicit locks in traditional
    concurrency methods.


 References:

    [1] Little Book of Semaphores (problem description on page 133)
        https://greenteapress.com/semaphores/LittleBookOfSemaphores.pdf

    [2] Composable Memory Transactions (PDF)
        https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf

-}


module MultipleBarbers (multipleBarbers) where

import Control.Concurrent.STM (atomically, retry, orElse, STM, TVar, newTVar, readTVar, readTVarIO, modifyTVar, writeTVar)
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
                    cash     :: Queue,          -- line-up to use the cash register
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
  cash     <- new []        -- nobody in line to use the cash register
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

  readTVarIO couch    >>= print . length
  readTVarIO standing >>= print . length
  readTVarIO barber1  >>= print
  readTVarIO barber2  >>= print
  readTVarIO barber3  >>= print

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
      now im Sleeping

      log $ "Barber " ++ show i ++ " sleeping..."

      waitWhile im Sleeping


    work :: IO ()
    work = do
      now im Cutting

      log $ "Barber " ++ show i ++ " cutting hair..."

      -- cuts take somewhere between 15 and 45 minutes
      roll 15 45 >>= wait

      -- after the cut, wait for our turn to use the cash register
      lineUpFor cash $ do

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


-- FIFO queue, callers will block until their turn at the end of the queue, then
-- execute the task they were waiting to do at the counter, then leave the queue

-- Note: there's a risk here that the queue will be modified arbitrarily outside this
-- function by any code with access to the queue, but if it's only modified by
-- this function then it should be safe

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
    1 Paying...
    Barber 1 done with customer
    Barber 1 cutting hair...
    1 Another satisfied customer!
    5 Sat down...
    6 Sat down...
    2 Paying...
    Barber 2 done with customer
    2 Another satisfied customer!
    Barber 2 cutting hair...
    7 Sat down...
    8 Sat down...
    9 Sat down...
    10 Standing...
    3 Paying...
    Barber 3 done with customer
    Barber 3 cutting hair...
    3 Another satisfied customer!
    11 Standing...
    12 Standing...
    4 Paying...
    Barber 1 done with customer
    Barber 1 cutting hair...
    4 Another satisfied customer!
    5 Paying...
    Barber 2 done with customer
    Barber 2 cutting hair...
    5 Another satisfied customer!
    13 Standing...
    14 Standing...
    15 Standing...
    16 Standing...
    7 Paying...
    7 Another satisfied customer!
    Barber 1 done with customer
    Barber 1 cutting hair...
    8 Paying...
    Barber 2 done with customer
    Barber 2 cutting hair...
    8 Another satisfied customer!
    17 Standing...
    18 Standing...
    6 Paying...
    Barber 3 done with customer
    Barber 3 cutting hair...
    6 Another satisfied customer!
    19 Standing...
    20 Standing...
    9 Paying...
    Barber 1 done with customer
    Barber 1 cutting hair...
    9 Another satisfied customer!
    11 Paying...
    Barber 1 done with customer
    Barber 1 cutting hair...
    11 Another satisfied customer!
    10 Paying...
    Barber 2 done with customer
    10 Another satisfied customer!
    Barber 2 cutting hair...
    16 Paying...
    Barber 1 done with customer
    Barber 1 cutting hair...
    16 Another satisfied customer!
    12 Paying...
    Barber 3 done with customer
    Barber 3 cutting hair...
    12 Another satisfied customer!
    15 Paying...
    Barber 2 done with customer
    Barber 2 cutting hair...
    15 Another satisfied customer!
    18 Paying...
    18 Another satisfied customer!
    Barber 1 done with customer
    Barber 1 cutting hair...
    19 Paying...
    Barber 2 done with customer
    Barber 2 cutting hair...
    19 Another satisfied customer!
    20 Paying...
    Barber 3 done with customer
    Barber 3 cutting hair...
    20 Another satisfied customer!
    17 Paying...
    Barber 1 done with customer
    Barber 1 sleeping...
    17 Another satisfied customer!
    13 Paying...
    Barber 3 done with customer
    Barber 3 sleeping...
    13 Another satisfied customer!
    14 Paying...
    Barber 2 done with customer
    Barber 2 sleeping...
    14 Another satisfied customer!
    0
    0
    Sleeping
    Sleeping
    Sleeping


    To test the branch where customers are lost I reduced the wait time between customers
    to 1 minute and got this output:

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
    6 Sat down...
    7 Sat down...
    8 Standing...
    9 Standing...
    10 Standing...
    11 Standing...
    12 Standing...
    13 Standing...
    14 Standing...
    15 Standing...
    16 Lost customer
    17 Lost customer
    2 Paying...
    Barber 2 done with customer
    Barber 2 cutting hair...
    2 Another satisfied customer!
    18 Standing...
    19 Lost customer
    20 Lost customer
    3 Paying...
    Barber 3 done with customer
    3 Another satisfied customer!
    Barber 3 cutting hair...
    .
    .
    .

-}

