module Main (main) where

import Text.Read (readMaybe)

data TransactionKind = Income | Expense
  deriving (Show, Eq, Read)

data Category
  = Food
  | Rent
  | Transport
  | Salary
  | Entertainment
  | Other
  deriving (Show, Eq, Read)

data Transaction = Transaction
  { transactionId :: Int
  , date          :: String
  , kind          :: TransactionKind
  , category      :: Category
  , amount        :: Double
  , description   :: String
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

addTransaction :: Transaction -> Ledger -> Ledger
addTransaction t ledger = ledger <> [t]

nextId :: Ledger -> Int
nextId [] = 1
nextId ts = maximum (map transactionId ts) + 1

deleteById :: Int -> Ledger -> Ledger
deleteById tid ledger =
  filter (\t -> transactionId t /= tid) ledger

showTotals :: Ledger -> IO ()
showTotals ledger = do
  let inc = totalIncome ledger
      exp = totalExpense ledger
      net = netBalance ledger
  putStrLn $ "Total income:  " ++ show inc
  putStrLn $ "Total expense: " ++ show exp
  putStrLn $ "Net balance:   " ++ show net

readInt :: String -> IO (Maybe Int)
readInt prompt = do
  putStrLn prompt
  input <- getLine
  return (readMaybe input)

readDouble :: String -> IO (Maybe Double)
readDouble prompt = do
  putStrLn prompt
  input <- getLine
  return (readMaybe input)

addTransactionInteractive :: Ledger -> IO Ledger
addTransactionInteractive ledger = do
  putStrLn "Add new transaction"

  putStrLn "Date (yyyy-mm-dd):"
  d <- getLine

  putStrLn "Kind (Income or Expense):"
  kindStr <- getLine

  putStrLn "Category (Food | Rent | Transport | Salary | Entertainment | Other):"
  catStr <- getLine

  mAmount <- readDouble "Amount:"
  putStrLn "Description:"
  desc <- getLine

  case (readMaybe kindStr :: Maybe TransactionKind,
        readMaybe catStr  :: Maybe Category,
        mAmount) of
    (Just k, Just c, Just a) -> do
      let newId = nextId ledger
          t = Transaction newId d k c a desc
          updated = addTransaction t ledger
      putStrLn "Transaction added:"
      print t
      return updated
    _ -> do
      putStrLn "Invalid input. Transaction not added."
      return ledger

loop :: Ledger -> IO ()
loop ledger = do
  putStrLn ""
  putStrLn "WhoCares Personal Finance Tracker"
  putStrLn "1. View ledger"
  putStrLn "2. View totals"
  putStrLn "3. Delete transaction by id"
  putStrLn "4. Add transaction"
  putStrLn "0. Exit"
  putStrLn "Choose an option:"
  choice <- getLine
  case choice of
    "1" -> do
      mapM_ print ledger
      loop ledger
    "2" -> do
      showTotals ledger
      loop ledger
    "3" -> do
      mId <- readInt "Enter transaction id to delete:"
      case mId of
        Nothing -> do
          putStrLn "Invalid id."
          loop ledger
        Just tid -> do
          let updated = deleteById tid ledger
          putStrLn "Updated ledger:"
          mapM_ print updated
          loop updated
    "4" -> do
      updated <- addTransactionInteractive ledger
      loop updated
    "0" ->
      putStrLn "Goodbye."
    _ -> do
      putStrLn "Invalid option."
      loop ledger

main :: IO ()
main = loop sampleLedger
