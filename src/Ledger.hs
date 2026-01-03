{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Ledger
Description : Core Business Logic and Data Types
Copyright   : (c) Student, 2024
License     : AllRightsReserved

This module defines the core data structures and business logic for the application.
It demonstrates:
  * Custom Data Types (Transaction, Category)
  * TypeClasses (HasId)
  * Custom Monad (ValidationResult) for error handling
  * Pure functional logic for calculations
  * CSV persistence using the 'cassava' library
-}

module Ledger
  ( TransactionKind(..)
  , Category(..)
  , Transaction(..)
  , Ledger
  , sampleLedger
  , HasId(..)
  , ValidationResult(..)
  , totalIncome
  , totalExpense
  , netBalance
  , highestExpense
  , addTransaction
  , nextId
  , deleteById
  , validateDelete
  , computeBalances
  , categoryListString
  , validateDate
  , saveLedgerToCsv
  , loadLedgerFromCsv
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Csv
import Control.Exception (try, IOException)
import Data.Time (Day)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)

-- Core data types

data TransactionKind = Income | Expense
  deriving (Show, Eq, Read)

data Category
  = Food
  | Rent
  | Transport
  | Salary
  | Entertainment
  | Utilities
  | Healthcare
  | Education
  | Shopping
  | Investment
  | Travel
  | Other
  deriving (Show, Eq, Read, Enum, Bounded)

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
  [ Transaction 1 "01-01-2024" Income  Salary        5000 "Monthly Salary"
  , Transaction 2 "02-01-2024" Expense Food            50 "Groceries"
  , Transaction 3 "03-01-2024" Expense Rent          1500 "January Rent"
  , Transaction 4 "04-01-2024" Expense Transport      100 "Monthly Metro Card"
  , Transaction 5 "05-01-2024" Expense Entertainment   75 "Concert Ticket"
  , Transaction 6 "06-01-2024" Expense Utilities      120 "Electricity Bill"
  , Transaction 7 "07-01-2024" Expense Shopping       200 "New Clothes"
  ]

-- | 'HasId' TypeClass
-- Demonstrates polymorphism. Any type implementing this class can be identified by an Int ID.
-- This allows us to write generic functions like 'deleteById' that work on any record with an ID.
class HasId a where
  getId :: a -> Int

instance HasId Transaction where
  getId = transactionId

-- | 'ValidationResult' Monad
-- A custom Monad to encapsulate success (Ok) or failure (Error).
-- This demonstrates the implementation of Functor, Applicative, and Monad typeclasses
-- to handle side-effect-free error propagation.
data ValidationResult a
  = Ok a
  | Error String
  deriving (Show, Eq)

instance Functor ValidationResult where
  fmap f (Ok a)    = Ok (f a)
  fmap _ (Error e) = Error e

instance Applicative ValidationResult where
  pure = Ok
  Ok f <*> Ok a    = Ok (f a)
  Error e <*> _    = Error e
  _ <*> Error e    = Error e

instance Monad ValidationResult where
  Ok a   >>= f = f a
  Error e >>= _ = Error e

-- CSV instances

instance ToNamedRecord Transaction where
  toNamedRecord t =
    namedRecord
      [ "Id"          .= transactionId t
      , "Date"        .= date t
      , "Kind"        .= show (kind t)
      , "Category"    .= show (category t)
      , "Amount"      .= amount t
      , "Description" .= description t
      ]

instance FromNamedRecord Transaction where
  parseNamedRecord m =
    Transaction
      <$> m .: "Id"
      <*> m .: "Date"
      <*> (read <$> m .: "Kind")
      <*> (read <$> m .: "Category")
      <*> m .: "Amount"
      <*> m .: "Description"

instance DefaultOrdered Transaction where
  headerOrder _ =
    header ["Id", "Date", "Kind", "Category", "Amount", "Description"]

-- Pure analytics

totalIncome :: Ledger -> Double
totalIncome ledger =
  sum [ amount t | t <- ledger, kind t == Income ]

totalExpense :: Ledger -> Double
totalExpense ledger =
  sum [ amount t | t <- ledger, kind t == Expense ]

netBalance :: Ledger -> Double
netBalance ledger =
  totalIncome ledger - totalExpense ledger

highestExpense :: Ledger -> Maybe Transaction
highestExpense ledger =
  case filter (\t -> kind t == Expense) ledger of
    []    -> Nothing
    exps  -> Just (maximumByAmount exps)
  where
    maximumByAmount (x:xs) = foldr (\t acc -> if amount t > amount acc then t else acc) x xs
    maximumByAmount []     = error "maximumByAmount on empty list"

addTransaction :: Transaction -> Ledger -> Ledger
addTransaction t ledger = ledger ++ [t]

nextId :: Ledger -> Int
nextId []     = 1
nextId ledger = maximum (map transactionId ledger) + 1

deleteById :: HasId a => Int -> [a] -> [a]
deleteById tid xs =
  filter (\x -> getId x /= tid) xs

validateDelete :: HasId a => Int -> [a] -> ValidationResult [a]
validateDelete tid xs =
  if any (\x -> getId x == tid) xs
    then Ok (deleteById tid xs)
    else Error "Id does not exist. Please try again."

computeBalances :: Ledger -> [(Transaction, Double)]
computeBalances ledger =
  reverse (snd (foldl step (0, []) ledger))
  where
    step (bal, acc) t =
      let delta = if kind t == Income then amount t else negate (amount t)
          bal'  = bal + delta
      in (bal', (t, bal') : acc)

-- Shared constants and validators

categoryListString :: String
categoryListString =
  "Food | Rent | Transport | Salary | Entertainment | Utilities | Healthcare | Education | Shopping | Investment | Travel | Other"

validateDate :: String -> Maybe String
validateDate s =
  case parseTimeM True defaultTimeLocale "%d-%m-%Y" s :: Maybe Day of
    Just day -> Just (formatTime defaultTimeLocale "%d-%m-%Y" day)
    Nothing  -> Nothing

-- Persistence

saveLedgerToCsv :: Ledger -> IO ()
saveLedgerToCsv ledger = do
  let bytes = encodeDefaultOrderedByName ledger
  BL.writeFile "ledger.csv" bytes

loadLedgerFromCsv :: IO Ledger
loadLedgerFromCsv = do
  eBytes <- try (BL.readFile "ledger.csv") :: IO (Either IOException BL.ByteString)
  case eBytes of
    Left _ -> return sampleLedger
    Right bytes ->
      case decodeByName bytes of
        Left _       -> return sampleLedger
        Right (_, v) -> return (V.toList v)
