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
  [ Transaction 1 "2024-01-01" Income  Salary        5000 "Monthly Salary"
  , Transaction 2 "2024-01-02" Expense Food            50 "Groceries"
  , Transaction 3 "2024-01-03" Expense Rent          1500 "January Rent"
  , Transaction 4 "2024-01-04" Expense Transport      100 "Monthly Metro Card"
  , Transaction 5 "2024-01-05" Expense Entertainment   75 "Concert Ticket"
  ]

totalIncome :: Ledger -> Double
totalIncome ts =
  sum [ amount t | t <- ts, kind t == Income ]

totalExpense :: Ledger -> Double
totalExpense ts =
  sum [ amount t | t <- ts, kind t == Expense ]

netBalance :: Ledger -> Double
netBalance ts = totalIncome ts - totalExpense ts

main :: IO ()
main = do
  putStrLn "Ledger:"
  mapM_ print sampleLedger

  let inc  = totalIncome sampleLedger
      exp  = totalExpense sampleLedger
      net  = netBalance sampleLedger

  putStrLn ""
  putStrLn $ "Total income:  " ++ show inc
  putStrLn $ "Total expense: " ++ show exp
  putStrLn $ "Net balance:   " ++ show net
