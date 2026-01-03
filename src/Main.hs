{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
Description : WhoCares Personal Finance Tracker Entry Point
Copyright   : (c) Student, 2024
License     : AllRightsReserved

This module handles the User Interface (UI) and interaction logic.
It utilizes the 'Ledger' module for core data types and business logic.
Key features:
  * Command-line menu navigation
  * Interactive data entry with validation
  * Reporting and visualization of financial data
-}

module Main (main) where

import Text.Read (readMaybe)
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Printf (printf)

import Ledger

-- Formatting helpers

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

showRowWithBalance :: (Transaction, Double) -> String
showRowWithBalance (t, bal) =
  pad 4  (show (transactionId t)) ++ " | "
  ++ pad 10 (date t)              ++ " | "
  ++ pad 8  (show (kind t))       ++ " | "
  ++ pad 13 (show (category t))   ++ " | "
  ++ pad 10 (printf "%.2f" (amount t)) ++ " | "
  ++ pad 10 (printf "%.2f" bal)   ++ " | "
  ++ description t

printTable :: Ledger -> IO ()
printTable ledger = do
  putStrLn ""
  putStrLn "ID   | Date       | Kind     | Category      | Amount     | Description"
  putStrLn "-----------------------------------------------------------------------"
  mapM_ (putStrLn . showRow) ledger

printTableWithRunning :: Ledger -> IO ()
printTableWithRunning ledger = do
  putStrLn ""
  putStrLn "ID   | Date       | Kind     | Category      | Amount     | Balance    | Description"
  putStrLn "-------------------------------------------------------------------------------"
  mapM_ (putStrLn . showRowWithBalance) (computeBalances ledger)

showTotals :: Ledger -> IO ()
showTotals ledger = do
  let inc      = totalIncome ledger
      exps     = totalExpense ledger
      net      = netBalance ledger
      count    = length ledger
      incomes  = filter (\t -> kind t == Income) ledger
      expenses = filter (\t -> kind t == Expense) ledger
      avgAmount xs =
        if null xs
          then 0
          else sum (map amount xs) / fromIntegral (length xs)
      avgInc  = avgAmount incomes
      avgExp  = avgAmount expenses
      mHigh   = highestExpense ledger

  putStrLn "----------------------------------------"
  putStrLn "Account totals"
  putStrLn "----------------------------------------"
  putStrLn $ "Number of transactions: " ++ show count
  putStrLn $ "Total income:           " ++ printf "%.2f" inc
  putStrLn $ "Total expenses:         " ++ printf "%.2f" exps
  putStrLn $ "Net balance:            " ++ printf "%.2f" net
  putStrLn $ "Average income:         " ++ printf "%.2f" avgInc
  putStrLn $ "Average expense:        " ++ printf "%.2f" avgExp

  case mHigh of
    Nothing -> do
      putStrLn ""
      putStrLn "Highest expense: none recorded yet."
    Just t  -> do
      putStrLn ""
      putStrLn "Highest expense:"
      putStrLn $ "  Amount:      " ++ printf "%.2f" (amount t)
      putStrLn $ "  Date:        " ++ date t
      putStrLn $ "  Category:    " ++ show (category t)
      putStrLn $ "  Description: " ++ description t

-- Input helpers

readInt :: String -> IO (Maybe Int)
readInt prompt = do
  putStrLn prompt
  input <- getLine
  return (readMaybe input)

promptUntilValid :: Read a => String -> String -> IO a
promptUntilValid prompt errorMessage = do
  putStrLn prompt
  input <- getLine
  case readMaybe input of
    Just val -> return val
    Nothing  -> do
      putStrLn errorMessage
      promptUntilValid prompt errorMessage

promptDateWithDefault :: String -> String -> IO String
promptDateWithDefault label today = do
  putStrLn $ label ++ " [Press Enter for today: " ++ today ++ "]:"
  input <- getLine
  if null input
    then return today
    else case validateDate input of
      Just d  -> return d
      Nothing -> do
        putStrLn "Invalid date. Please use dd-mm-yyyy (for example 05-03-2024)."
        promptDateWithDefault label today

promptDateKeep :: String -> String -> IO String
promptDateKeep label current = do
  putStrLn $ label ++ " [" ++ current ++ "] (press Enter to keep):"
  input <- getLine
  if null input
    then return current
    else case validateDate input of
      Just d  -> return d
      Nothing -> do
        putStrLn "Invalid date. Please use dd-mm-yyyy (for example 05-03-2024)."
        promptDateKeep label current

promptPositiveAmount :: String -> IO Double
promptPositiveAmount label = do
  putStrLn label
  input <- getLine
  case readMaybe input :: Maybe Double of
    Nothing -> do
      putStrLn "Invalid amount. Please enter a number (for example 100 or 99.50)."
      promptPositiveAmount label
    Just x ->
      if x <= 0
        then do
          putStrLn "Amount must be greater than 0."
          promptPositiveAmount label
        else return x

promptPositiveAmountKeep :: String -> Double -> IO Double
promptPositiveAmountKeep label current = do
  putStrLn $ label ++ " [" ++ show current ++ "] (press Enter to keep):"
  input <- getLine
  if null input
    then return current
    else case readMaybe input :: Maybe Double of
      Nothing -> do
        putStrLn "Invalid amount. Please enter a number (for example 100 or 99.50)."
        promptPositiveAmountKeep label current
      Just x ->
        if x <= 0
          then do
            putStrLn "Amount must be greater than 0."
            promptPositiveAmountKeep label current
          else return x

promptFor :: String -> String -> IO String
promptFor label currentValue = do
  putStrLn $ label ++ " [" ++ currentValue ++ "] (press Enter to keep):"
  getLine

promptRead :: (Read a, Show a) => String -> a -> IO a
promptRead label current = do
  input <- promptFor label (show current)
  case readMaybe input of
    Just val -> return val
    Nothing  -> return current

-- Interactive operations

addTransactionInteractive :: Ledger -> IO Ledger
addTransactionInteractive ledger = do
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn "Add new transaction"
  putStrLn "----------------------------------------"

  now <- getCurrentTime
  let today = formatTime defaultTimeLocale "%d-%m-%Y" (utctDay now)

  d <- promptDateWithDefault "Date (dd-mm-yyyy)" today

  k <- promptUntilValid
         "Kind (Income or Expense):"
         "Invalid kind. Please enter \"Income\" or \"Expense\"."

  c <- promptUntilValid
         ("Category (" ++ categoryListString ++ "):")
         "Invalid category. Please choose from the list."

  putStrLn "Description:"
  desc <- getLine

  a <- promptPositiveAmount "Amount:"

  let newId   = nextId ledger
      t       = Transaction newId d k c a desc
      updated = addTransaction t ledger

  putStrLn ""
  putStrLn "Transaction added successfully:"
  putStrLn "ID   | Date       | Kind     | Category      | Amount     | Description"
  putStrLn "-----------------------------------------------------------------------"
  putStrLn (showRow t)
  return updated

editTransactionInteractive :: Ledger -> IO Ledger
editTransactionInteractive ledger = do
  mId <- readInt "Enter the transaction id you want to edit:"
  case mId of
    Nothing -> do
      putStrLn "Invalid id. Please enter a whole number such as 1 or 2."
      editTransactionInteractive ledger
    Just tid -> do
      case filter (\t -> transactionId t == tid) ledger of
        [] -> do
          putStrLn "Id does not exist. Please try again."
          editTransactionInteractive ledger
        (oldT:_) -> do
          putStrLn ""
          putStrLn "Editing transaction. Leave a field empty to keep its current value."

          newDate     <- promptDateKeep "Date" (date oldT)
          newKind     <- promptRead "Kind (Income/Expense)" (kind oldT)
          newCategory <- promptRead "Category"              (category oldT)

          descInput <- promptFor "Description" (description oldT)
          let newDesc = if null descInput then description oldT else descInput

          newAmount <- promptPositiveAmountKeep "Amount" (amount oldT)

          let newT = oldT { date        = newDate
                          , kind        = newKind
                          , category    = newCategory
                          , description = newDesc
                          , amount      = newAmount
                          }

          if newT == oldT
            then do
              putStrLn ""
              putStrLn "No changes made."
              return ledger
            else do
              putStrLn ""
              putStrLn "Transaction updated successfully:"
              putStrLn "ID   | Date       | Kind     | Category      | Amount     | Description"
              putStrLn "-----------------------------------------------------------------------"
              putStrLn (showRow newT)
              return (addTransaction newT (deleteById tid ledger))

deleteTransactionInteractive :: Ledger -> IO Ledger
deleteTransactionInteractive ledger = do
  mId <- readInt "Enter transaction id to delete:"
  case mId of
    Nothing -> do
      putStrLn "Invalid id. Please enter a number."
      deleteTransactionInteractive ledger
    Just tid -> do
      let result = validateDelete tid ledger
      case result of
        Error msg -> do
          putStrLn msg
          deleteTransactionInteractive ledger
        Ok updated -> do
          putStrLn ""
          putStrLn "Updated ledger:"
          printTable updated
          return updated

viewByCategoryInteractive :: Ledger -> IO ()
viewByCategoryInteractive ledger = do
  putStrLn ""
  putStrLn "----------------------------------------"
  putStrLn "View by Category"
  putStrLn "----------------------------------------"

  c <- promptUntilValid ("Category (" ++ categoryListString ++ "):")
                        "Invalid Category. Please choose from the list."

  let filtered = filter (\t -> category t == c) ledger

  putStrLn ""
  putStrLn $ "Transactions for category: " ++ show c
  printTable filtered

viewCategoryDistribution :: Ledger -> IO ()
viewCategoryDistribution ledger = do
  let expenses = filter (\t -> kind t == Expense) ledger
      incomes  = filter (\t -> kind t == Income) ledger
      totalExp = sum (map amount expenses)
      totalInc = sum (map amount incomes)
      allCats  = [minBound .. maxBound] :: [Category]

      calcSum :: [Transaction] -> Category -> Double
      calcSum ts c = sum [amount t | t <- ts, category t == c]

      printDist :: String -> Double -> [Transaction] -> IO ()
      printDist title total ts = do
        putStrLn ""
        putStrLn "----------------------------------------"
        putStrLn $ title ++ " Distribution"
        putStrLn "----------------------------------------"
        putStrLn "Category        | Amount     | %"
        putStrLn "----------------------------------------"
        mapM_ (\c -> do
          let s = calcSum ts c
          if s > 0
            then do
              let pct = if total > 0 then (s / total) * 100 else 0
              putStrLn $ pad 15 (show c) ++ " | "
                         ++ pad 10 (printf "%.2f" s) ++ " | "
                         ++ printf "%.1f%%" pct
            else return ()
          ) allCats
        putStrLn "----------------------------------------"
        putStrLn $ "Total:          " ++ printf "%.2f" total

  printDist "Expense" totalExp expenses
  printDist "Income"  totalInc incomes

viewTransactionsInteractive :: Ledger -> IO ()
viewTransactionsInteractive ledger = do
  putStrLn ""
  putStrLn "Current ledger (with running balance):"
  printTableWithRunning ledger

-- Menu and main loop

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
  putStrLn "6. View by category"
  putStrLn "7. View category distribution"
  putStrLn "0. Exit and save"
  putStrLn "----------------------------------------"
  putStrLn "Please choose an option (0-7):"

loop :: Ledger -> IO Ledger
loop ledger = do
  printMenu
  choice <- getLine
  case choice of
    "1" -> do
      viewTransactionsInteractive ledger
      loop ledger
    "2" -> do
      updated <- addTransactionInteractive ledger
      loop updated
    "3" -> do
      updated <- editTransactionInteractive ledger
      loop updated
    "4" -> do
      updated <- deleteTransactionInteractive ledger
      loop updated
    "5" -> do
      showTotals ledger
      loop ledger
    "6" -> do
      viewByCategoryInteractive ledger
      loop ledger
    "7" -> do
      viewCategoryDistribution ledger
      loop ledger
    "0" -> do
      putStrLn "Saving your data and exiting. Goodbye."
      return ledger
    _ -> do
      putStrLn ("Invalid option: \"" ++ choice ++ "\". Please choose a number from 0 to 7.")
      loop ledger

main :: IO ()
main = do
  initial <- loadLedgerFromCsv
  final   <- loop initial
  saveLedgerToCsv final
