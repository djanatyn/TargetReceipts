{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TargetReceipts where

import           Data.Yaml
import           GHC.Generics

import           Control.Applicative
import           Control.Monad

import qualified Data.Text.IO            as TIO

import           Text.Pandoc             (def, handleError, runIO)
import           Text.Pandoc.Builder
import           Text.Pandoc.Writers
import           Text.Printf

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Simple

import           Data.Aeson
import           Data.Aeson.Types

import           System.Environment      (getArgs)

type DPCI = String
type USD = Double

data Item = Item
  { name  :: String
  , price :: USD
  , url   :: String
  } deriving Show

data Purchase = Purchase
  { dpci     :: String
  , discount :: Maybe USD
  } deriving Show

instance FromJSON Purchase where
  parseJSON = withObject "purchase" $ \o -> do
    dpci     <- o .: "dpci"
    discount <- o .:? "discount"
    return Purchase{..}


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
    price <- item .: "list_price" >>= (.: "max_price")
    url   <- item .: "url"

    return (title, price, url)

fetchItem :: Purchase -> IO Item
fetchItem purchase = do
  response <- searchDPCI $ dpci purchase
  
  let item = case parseResponse response of
              Success [(name, price, url)] -> case discount purchase of
                Nothing -> Item{..}
                Just offset -> Item{..} { price = price + offset }
              _ -> error $ "could not fetch item (" ++ show purchase ++ ")"
      validURL = ("https://www.target.com" ++ url item) in
        return $ item { url = validURL }

-- calculate total price incrementally with fold
totalCost :: [Item] -> [USD]
totalCost = scanl (+) 0 . map price

priceTable :: [Item] -> Pandoc
priceTable items = doc $ simpleTable header rows
  where createRow :: Item -> USD -> [Blocks]
        createRow item totalSum = [ plain $ str $ name item
                                  , plain $ str $ url item
                                  , plain $ str $ formatCost $ price item
                                  , plain $ str $ formatCost totalSum ]
        header = [plain "Item", plain "URL", plain "Cost", plain "Sum"]
        rows = zipWith createRow items (tail $ totalCost items)

-- pandoc table rendering
formatCost :: USD -> String
formatCost = printf "$%0.2f"

loadPurchases :: FilePath -> IO [Purchase]
loadPurchases path = either (error . show) id <$> decodeFileEither path

main :: IO ()
main = do
  args <- getArgs
  let path = unwords args

  items <- loadPurchases path >>= mapM fetchItem
  
  result <- runIO (writeOrg def (priceTable items)) >>= handleError
  TIO.putStrLn result
