{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TargetReceipts where

import Data.Yaml
import GHC.Generics

import Control.Applicative
import Control.Monad

import qualified Data.Text.IO as TIO

import Text.Pandoc (def, runIO, handleError)
import Text.Pandoc.Builder
import Text.Pandoc.Writers
import Text.Printf

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple

import Data.Aeson
import Data.Aeson.Types

import System.Environment (getArgs)

type DPCI = String
type USD = Double
data Item = Item
  { name :: String
  , price :: USD
  } deriving (Generic, Show)

instance ToJSON Item

instance FromJSON Item


searchDPCI :: DPCI -> IO Object
searchDPCI filter = do
  manager <- newManager tlsManagerSettings
  
  parseRequest url >>= httpJSON >>= (return . getResponseBody) where
    url = "https://redsky.target.com/v1/plp/search/?count=1&keyword=" ++ filter

-- do { response <- searchDPCI "211080031"; return $ parseResponse response }
-- Success [("Naked Green Machine All Natural Fruit + Boosts Juice Smoothie - 15.2oz",2.79,"/p/naked-green-machine-all-natural-fruit-boosts-juice-smoothie-15-2oz/-/A-13397596")]

parseResponse :: Object -> Result [(String, USD, String)]
parseResponse = parse $ (.: "search_response") >=> (.: "items") >=> (.: "Item") >=> (mapM parseItem) where
  parseItem item = do
    title <- item .: "title"
    price <- item .: "list_price" >>= (.: "price")
    url   <- item .: "url"
    
    return (title, price, url)

-- calculate total price incrementally with fold
totalCost :: [Item] -> [USD]
totalCost = scanl (+) 0 . map price

priceTable :: [Item] -> Pandoc
priceTable items = doc $ simpleTable header rows
  where createRow :: Item -> USD -> [Blocks]
        createRow item totalSum = [ plain $ str $ name item
                                  , plain $ str $ formatCost $ price item
                                  , plain $ str $ formatCost totalSum ] 
        header = [plain "Item", plain "Cost", plain "Sum"]
        rows = zipWith createRow items (tail $ totalCost items)

-- pandoc table rendering
formatCost :: USD -> String
formatCost = printf "$%0.2f"

loadItems :: FilePath -> IO [Item]
loadItems path = either (error . show) id <$> decodeFileEither path

main :: IO ()
main = do
  args <- getArgs
  let path = unwords args

  items <- loadItems path
  result <- runIO (writeRST def (priceTable items)) >>= handleError

  TIO.putStrLn result
