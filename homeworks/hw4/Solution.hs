module Solution where

newtype Reader r a = Reader { runReader :: r -> a }

-- Task 1
instance Functor (Reader r) where
    fmap f (Reader g) = Reader $ \r -> f (g r)

instance Applicative (Reader r) where
    pure x = Reader $ \_ -> x
    liftA2 f (Reader ra) (Reader rb) = Reader $ \r -> f (ra r) (rb r)

instance Monad (Reader r) where
    (Reader ra) >>= f = Reader $ \r ->
        let a = ra r
            (Reader rb) = f a
        in rb r

-- Task 2
ask :: Reader r r
ask = Reader $ \r -> r

asks  :: (r -> a) -> Reader r a
asks f = Reader $ \r -> f r

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader ra) = Reader $ \r ->
    let modifiedR = f r
    in ra modifiedR


-- Task 3
data BankConfig = BankConfig
  { interestRate   :: Double  -- annual interest rate (e.g. 0.05 for 5%)
  , transactionFee :: Int     -- flat fee charged per transaction
  , minimumBalance :: Int     -- minimum required balance on an account
  } deriving (Show)

data Account = Account
  { accountId :: String       -- account identifier
  , balance   :: Int          -- current balance
  } deriving (Show)

calculateInterest   :: Account -> Reader BankConfig Int
calculateInterest account = do
    rate <- asks interestRate
    return $ floor (fromIntegral (balance account) * rate)

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    fee <- asks transactionFee
    return $ account { balance = balance account - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    minBalance <- asks minimumBalance
    return $ balance account >= minBalance

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    updatedAcc <- applyTransactionFee account
    interest <- calculateInterest updatedAcc
    meetsMinBalance <- checkMinimumBalance updatedAcc
    return (updatedAcc, interest, meetsMinBalance)