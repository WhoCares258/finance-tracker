module Main (main) where

data Transaction = Transaction
  { amount :: Double
  , description :: String
  } deriving (Show, Eq)

main :: IO ()
main = do
  let t = Transaction 123.45 "Coffee"
  putStrLn "Here is a transaction:"
  print t