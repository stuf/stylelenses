{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Data.Aeson
import           Data.Aeson.Lens
import           Control.Lens
import           Types

--

jsonFile :: FilePath
jsonFile = "data/text-styles.json"

getJSON :: IO L.ByteString
getJSON = L.readFile jsonFile

decodeJSON :: IO (Either String Styles)
decodeJSON = fmap eitherDecode getJSON

main :: IO ()
main = do
  d <- do
    d' <- decodeJSON
    case d' of
      Left _ -> fail "Could not parse styles JSON file"
      Right x -> return x

  print d
