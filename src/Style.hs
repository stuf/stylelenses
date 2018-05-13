{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Style ( Block(..)
             , Align(..)
             , Weight(..)
             , Media(..)
             , Style(..)
             ) where

data Block = Heading Int | Category | Body | Hero | Label | Paragraph | Other
  deriving (Show)

data Align = Left | Center | Right | Default
  deriving (Show)

data Weight = Hairline | Thin | Light | Regular | Book | Bold | Black
  deriving (Show)

data Media = Desktop | Mobile
  deriving (Show)

data Style =
  Style { _block :: Block
        , _alignment :: Align
        , _weight :: Weight
        , _media :: Media
        } deriving (Show)

