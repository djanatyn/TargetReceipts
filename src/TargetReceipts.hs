{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module TargetReceipts where

import           Data.Either
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics

import           Control.Applicative
import           Control.Monad

import qualified Data.Text.IO            as TIO
import qualified Data.Text as T

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
  { name     :: String
  , price    :: USD
  , url      :: String
  , itemDPCI :: DPCI
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

parseResponse :: Object -> Result [(String, USD, DPCI, String)]
parseResponse = parse $ (.: "search_response") >=> (.: "items") >=> (.: "Item") >=> (mapM parseItem) where
  parseItem item = do
    title     <- item .: "title"

    listPrice <- item .: "list_price" >>= (.:? "price")
    minPrice  <- item .: "list_price" >>= (.:? "min_price")
    maxPrice  <- item .: "list_price" >>= (.:? "max_price")

    itemDPCI  <- item .:? "dpci" .!= "N/A"
    url       <- item .: "url"

    let price = foldr1 max . catMaybes $ [listPrice, minPrice, maxPrice] in
      return (title, price, itemDPCI, "https://www.target.com" ++ url)

fetchItem :: Purchase -> IO Item
fetchItem purchase = do
  response <- searchDPCI $ dpci purchase

  let item = case parseResponse response of
              Success [(name, price, itemDPCI, url)] -> case discount purchase of
                Nothing     -> Item{..}
                Just offset -> Item{..} { price = price + offset }
              e -> error $ "could not fetch item (" ++ show purchase ++ ") " ++ (show e)
      validURL = ("https://www.target.com" ++ url item) in
        return $ item { url = validURL }

fetchMaybeItem :: Purchase -> IO (Either String Item)
fetchMaybeItem purchase = do
  response <- searchDPCI $ dpci purchase

  let item = case parseResponse response of
              Success [(name, price, itemDPCI, url)] -> case discount purchase of
                Nothing     -> Right Item{..}
                Just offset -> Right Item{..} { price = price + offset }
              e -> Left $ "could not fetch item (" ++ show purchase ++ ") " ++ (show e) in
    return item

-- calculate total price incrementally with fold
totalCost :: [Item] -> [USD]
totalCost = scanl (+) 0 . map price

priceTable :: [Item] -> Pandoc
priceTable items = doc $ simpleTable header rows
  where createRow :: Item -> USD -> [Blocks]
        createRow item totalSum = [ plain $ str $ T.pack $ name item
                                  , plain $ str $ T.pack $ itemDPCI item
                                  , plain $ str $ T.pack $ url item
                                  , plain $ str $ T.pack $ formatCost $ price item
                                  , plain $ str $ T.pack $ formatCost totalSum ]
        header = [plain "Item", plain "DPCI", plain "URL", plain "Cost", plain "Sum"]
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

  items <- loadPurchases path >>= mapM fetchMaybeItem

  print (lefts items)
  result <- runIO (writeOrg def (priceTable (rights items))) >>= handleError
  TIO.putStrLn result
