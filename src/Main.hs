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

sampleLedger :: Ledger
sampleLedger =
  [ Transaction 1 "2024-01-01" Income Salary        5000 "Monthly Salary"
  , Transaction 2 "2024-01-02" Expense Food           50 "Groceries"
  , Transaction 3 "2024-01-03" Expense Rent         1500 "January Rent"
  , Transaction 4 "2024-01-04" Expense Transport     100 "Monthly Metro Card"
  , Transaction 5 "2024-01-05" Expense Entertainment  75 "Concert Ticket"
  ]

calculateBalance :: Ledger -> Double
calculateBalance =
  sum . map signed
  where
    signed t =
      case kind t of
        Income  -> amount t
        Expense -> - amount t

main :: IO ()
main = do
  putStrLn "Ledger:"
  mapM_ print sampleLedger
  putStrLn "Net balance:"
  print (calculateBalance sampleLedger)
