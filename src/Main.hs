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

pad :: Int -> String -> String
pad n s =
  take n (s ++ repeat ' ')

showRow :: Transaction -> String
showRow t =
  pad 4  (show (transactionId t)) ++ " | "
  ++ pad 10 (date t)              ++ " | "
  ++ pad 8  (show (kind t))       ++ " | "
  ++ pad 13 (show (category t))   ++ " | "
  ++ pad 10 (show (amount t))     ++ " | "
  ++ description t

printTable :: Ledger -> IO ()
printTable ledger = do
  putStrLn ""
  putStrLn "ID   | Date       | Kind     | Category      | Amount     | Description"
  putStrLn "-----------------------------------------------------------------------"
  mapM_ (putStrLn . showRow) ledger

class HasId a where
  getId :: a -> Int

instance HasId Transaction where
  getId = transactionId

data ValidationResult a
  = Ok a
  | Error String
  deriving (Show, Eq)

instance Functor ValidationResult where
  fmap f (Ok a)     = Ok (f a)
  fmap _ (Error e)  = Error e

instance Applicative ValidationResult where
  pure = Ok
  Ok f <*> Ok a     = Ok (f a)
  Error e <*> _     = Error e
  _ <*> Error e     = Error e

instance Monad ValidationResult where
  Ok a >>= f        = f a
  Error e >>= _     = Error e

totalIncome :: Ledger -> Double
totalIncome ts =
  sum [ amount t | t <- ts, kind t == Income ]

totalExpense :: Ledger -> Double
totalExpense ts =
  sum [ amount t | t <- ts, kind t == Expense ]

netBalance :: Ledger -> Double
netBalance ts = totalIncome ts - totalExpense ts

addTransaction :: Transaction -> Ledger -> Ledger
addTransaction t ledger = ledger ++ [t]

nextId :: Ledger -> Int
nextId [] = 1
nextId ts = maximum (map transactionId ts) + 1

deleteById :: HasId a => Int -> [a] -> [a]
deleteById tid xs =
  filter (\x -> getId x /= tid) xs

validateDelete :: HasId a => Int -> [a] -> ValidationResult [a]
validateDelete tid xs =
  if any (\x -> getId x == tid) xs
    then Ok (deleteById tid xs)
    else Error "Transaction id not found."

showTotals :: Ledger -> IO ()
showTotals ledger = do
  let inc = totalIncome ledger
      exp = totalExpense ledger
      net = netBalance ledger
  putStrLn "----------------------------------------"
  putStrLn "Totals"
  putStrLn "----------------------------------------"
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
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn "Add new transaction"
  putStrLn "----------------------------------------"

  putStrLn "Date (yyyy-mm-dd):"
  d <- getLine

  putStrLn "Kind (Income or Expense):"
  kindStr <- getLine

  putStrLn "Category (Food | Rent | Transport | Salary | Entertainment | Other):"
  catStr <- getLine

  putStrLn "Description:"
  desc <- getLine

  mAmount <- readDouble "Amount:"

  case ( readMaybe kindStr :: Maybe TransactionKind
       , readMaybe catStr  :: Maybe Category
       , mAmount
       ) of
    (Just k, Just c, Just a) -> do
      let newId   = nextId ledger
          t       = Transaction newId d k c a desc
          updated = addTransaction t ledger
      putStrLn ""
      putStrLn "Transaction added:"
      putStrLn "ID   | Date       | Kind     | Category      | Amount     | Description"
      putStrLn "-----------------------------------------------------------------------"
      putStrLn (showRow t)
      return updated
    _ -> do
      putStrLn ""
      putStrLn "Invalid input. Transaction not added."
      return ledger

updateTransaction :: Transaction -> Ledger -> Ledger
updateTransaction newT ledger =
  map (\t -> if transactionId t == transactionId newT then newT else t) ledger

promptFor :: String -> String -> IO String
promptFor label currentValue = do
  putStrLn $ label ++ " [" ++ currentValue ++ "]:"
  getLine

editTransactionInteractive :: Ledger -> IO Ledger
editTransactionInteractive ledger = do
  mId <- readInt "Enter transaction id to edit:"
  case mId of
    Nothing -> do
      putStrLn "Invalid id."
      return ledger
    Just tid -> do
      case filter (\t -> transactionId t == tid) ledger of
        [] -> do
          putStrLn "Transaction id not found."
          return ledger
        (oldT:_) -> do
          putStrLn "Press Enter to keep current value."
          
          dInput <- promptFor "Date" (date oldT)
          let newDate = if null dInput then date oldT else dInput

          kInput <- promptFor "Kind" (show (kind oldT))
          let newKind = case readMaybe kInput of
                          Just k -> k
                          Nothing -> kind oldT

          cInput <- promptFor "Category" (show (category oldT))
          let newCategory = case readMaybe cInput of
                              Just c -> c
                              Nothing -> category oldT

          descInput <- promptFor "Description" (description oldT)
          let newDesc = if null descInput then description oldT else descInput

          aInput <- promptFor "Amount" (show (amount oldT))
          let newAmount = case readMaybe aInput of
                            Just a -> a
                            Nothing -> amount oldT

          let newT = oldT { date = newDate
                          , kind = newKind
                          , category = newCategory
                          , description = newDesc
                          , amount = newAmount
                          }
          
          putStrLn ""
          putStrLn "Transaction updated:"
          putStrLn "ID   | Date       | Kind     | Category      | Amount     | Description"
          putStrLn "-----------------------------------------------------------------------"
          putStrLn (showRow newT)
          
          return (updateTransaction newT ledger)

printMenu :: IO ()
printMenu = do
  putStrLn ""
  putStrLn "========================================"
  putStrLn "WhoCares Personal Finance Tracker"
  putStrLn "========================================"
  putStrLn "1. View transactions"
  putStrLn "2. Add transaction"
  putStrLn "3. Edit transaction"
  putStrLn "4. Delete transaction"
  putStrLn "5. View totals"
  putStrLn "0. Exit"
  putStrLn "----------------------------------------"
  putStrLn "Choose an option:"

loop :: Ledger -> IO ()
loop ledger = do
  printMenu
  choice <- getLine
  case choice of
    "1" -> do
      putStrLn ""
      putStrLn "Current ledger:"
      printTable ledger
      loop ledger

    "2" -> do
      updated <- addTransactionInteractive ledger
      loop updated

    "3" -> do
      updated <- editTransactionInteractive ledger
      loop updated

    "4" -> do
      mId <- readInt "Enter transaction id to delete:"
      case mId of
        Nothing -> do
          putStrLn "Invalid id."
          loop ledger
        Just tid -> do
          let result = validateDelete tid ledger
          case result of
            Error msg -> do
              putStrLn msg
              loop ledger
            Ok updated -> do
              putStrLn ""
              putStrLn "Updated ledger:"
              printTable updated
              loop updated

    "5" -> do
      showTotals ledger
      loop ledger

    "0" ->
      putStrLn "Goodbye."

    _ -> do
      putStrLn "Invalid option."
      loop ledger

main :: IO ()
main = loop sampleLedger