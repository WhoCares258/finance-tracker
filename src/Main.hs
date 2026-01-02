module Main (main) where

data TransactionKind = Income | Expense
  deriving (Show, Eq)

data Category
  = Food
  | Rent
  | Transport
  | Salary
  | Entertainment
  | Other
  deriving (Show, Eq)

data Transaction = Transaction
  { id :: Int
  , date :: String
  , kind :: TransactionKind
  , category :: Category
  , amount :: Double
  , description :: String
  } deriving (Show, Eq)

type Ledger = [Transaction]

main :: IO ()
main = do
  putStrLn "--- Transaction Ledger ---"
  mapM_ print sampleLedger
  putStrLn "\n--- Net Balance ---"
  print (calculateBalance sampleLedger)
