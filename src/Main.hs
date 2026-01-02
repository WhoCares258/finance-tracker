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

sampleLedger :: [Transaction]
sampleLedger =
  [ Transaction 1 "2026-01-01" Income Salary 5000.0 "January Salary"
  , Transaction 2 "2026-01-02" Expense Rent 1200.0 "January Rent"
  , Transaction 3 "2026-01-05" Expense Food 50.0 "Groceries"
  ]

calculateBalance :: [Transaction] -> Double
calculateBalance = sum . map signedAmount
  where
    signedAmount t = case kind t of
      Income -> amount t
      Expense -> - (amount t)

main :: IO ()
main = do
  putStrLn "--- Transaction Ledger ---"
  mapM_ print sampleLedger
  putStrLn "\n--- Net Balance ---"
  print (calculateBalance sampleLedger)
