{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TargetCosts where

import Data.Yaml
import GHC.Generics

import Text.Pandoc (def)
import Text.Pandoc.Builder
import Text.Pandoc.Writers.Markdown (writeMarkdown)

import Text.Printf

import System.Environment (getArgs)

type Cost = Double

type ItemName = String

-- 3-tuple for table rows
type CostTableRow = (ItemName, Cost, Cost)

-- item ADT using generics to load from yaml file
data Item = Item
  { name :: String
  , price :: Double
  } deriving (Generic, Show)

instance ToJSON Item

instance FromJSON Item

-- calculate total price incrementally with fold
totalCost :: [Item] -> [Double]
totalCost = scanl (+) 0 . map price

-- build up a list of table rows (items, prices, sum cost)
priceTable :: [Item] -> [CostTableRow]
priceTable items = zip3 names prices sums
  where
    names = map name items
    prices = map price items
    sums = tail (totalCost items)

-- pandoc table rendering
formatCost :: Double -> String
formatCost = printf "$%0.2f"

priceRowBlock :: CostTableRow -> [Blocks]
priceRowBlock (item, price, sum) =
  [ plain $ str item
  , plain $ str $ formatCost price
  , plain $ str $ formatCost sum
  ]

buildDocument :: [CostTableRow] -> Pandoc
buildDocument rows =
  doc $ simpleTable [plain "Item", plain "Cost", plain "Sum"] rowBlocks
  where
    rowBlocks = map priceRowBlock rows

renderTable :: [CostTableRow] -> String
renderTable rows = (writeMarkdown def) $ buildDocument rows

-- generate table
affection :: String
affection = "<3 <3 <3"

getItems :: IO (Either ParseException [Item])
getItems = do
  args <- getArgs
  let path = unwords $ args
  decodeFileEither path :: IO (Either ParseException [Item])

main :: IO ()
main = do
  eitherItems <- getItems
  putStrLn $ output eitherItems
  putStrLn affection
  where
    output (Left _) = "failure"
    output (Right items) = renderTable $ priceTable items