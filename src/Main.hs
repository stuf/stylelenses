#!/usr/bin/env stack
{- stack
   script
   --resolver lts-11.8
   --package text,bytestring,aeson,unordered-containers,vector,lens
-}
{-# LANGUAGE OverloadedStrings,
             RecordWildCards,
             DeriveGeneric,
             TemplateHaskell #-}
module Main where

import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T
import           Data.Aeson
import           Data.Foldable
import           Control.Lens
import           GHC.Generics
import qualified Style                as S

--

jsonFile :: FilePath
jsonFile = "data/text-styles.1.json"

getJSON :: IO L.ByteString
getJSON = L.readFile jsonFile

decodeJSON :: IO (Either String [Style])
decodeJSON = fmap eitherDecode getJSON

-- Let's declare some types, man

newtype Color a = Color (a, a, a, a) deriving (Show)

data Style =
  Style { _name :: T.Text
        , _block :: S.Block
        , _font :: T.Text
        , _size :: Float
        , _spacing :: Float
        , _lineHeight :: Float
        , _paragraphSpacing :: Float
        , _alignment :: Int
        , _color :: (Float, Float, Float, Float)
        } deriving (Show)

newtype StyleRule =
  StyleRule { _element :: T.Text
            } deriving (Show, Generic, Eq)

instance FromJSON Style where
  parseJSON = withObject "style" $ \o -> do
    _name' <- o .: "name"

    let _name = _name'
        split = T.splitOn "/" _name
        block = head split

    -- Process blocks
    let _block = case block of
                   "Hero"      -> S.Hero
                   "Category"  -> S.Category
                   "Label"     -> S.Label
                   "Paragraph" -> S.Paragraph
                   "Body"      -> S.Body
                   _ | T.isPrefixOf "H" block ->
                       S.Heading l'
                         where l = T.drop 1 block
                               l' = read $ T.unpack l
                   _ -> S.Other

    _font <- o .: "font"
    _size <- o .: "size"
    _spacing <- o .: "spacing"
    _lineHeight <- o .: "lineHeight"
    _paragraphSpacing <- o .: "paragraphSpacing"
    _alignment <- o .: "alignment"

    -- Transform colors into a better representation in code
    color <- o .: "color"
    _color <- do
      red <- color .: "red"
      green <- color .: "green"
      blue <- color .: "blue"
      alpha <- color .: "alpha"

      return (red, green, blue, alpha)

    return Style{..}

makeLenses ''Style

type Styles = [Style]

-- Parse JSON

parseStyles :: IO Styles
parseStyles = do
  jsonResult <- decodeJSON
  return $ case jsonResult of
    Right x -> x
    _ -> []

--

main :: IO ()
main = do
  jsonData <- parseStyles
  forM_ jsonData print

  return ()
