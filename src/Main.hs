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
  putStrLn "Welcome to WhoCares Personal Finance Tracker!"

  let sampleLedger = 
        [ Transaction 1 "2024-01-01" Income Salary 5000 "Monthly Salary"
        , Transaction 2 "2024-01-02" Expense Food 50 "Groceries"
        , Transaction 3 "2024-01-03" Expense Rent 1500 "January Rent"
        , Transaction 4 "2024-01-04" Expense Transport 100 "Monthly Metro Card"
        , Transaction 5 "2024-01-05" Expense Entertainment 75 "Concert Ticket"
        ]
  
  print sampleLedger
  mapM_ print sampleLedger
  let totalIncome = sum [amount t | t <- sampleLedger, kind t == Income]
  let totalExpense = sum [amount t | t <- sampleLedger, kind t == Expense]
  putStrLn $ "Total Income: $" ++ show totalIncome
  putStrLn $ "Total Expense: $" ++ show totalExpense
  putStrLn $ "Net Balance: $" ++ show (totalIncome - totalExpense)
  
