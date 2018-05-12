{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, RecordWildCards, TemplateHaskell #-}
module Types where

import qualified Data.Text    as T
import           GHC.Generics
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens

--

data Style =
  Style { name :: T.Text
        , font :: T.Text
        , size :: Float
        , alignment :: Float
        , color :: (Float, Float, Float, Float)
        } deriving (Show, Generic, ToJSON, FromJSON)

--

newtype Styles =
  Styles { styles :: [Style]
         } deriving (Show, Generic, ToJSON, FromJSON)

newtype Styles' =
  Styles' { _styles :: [Style]
          } deriving (Show, Generic)

instance FromJSON Styles' where
  parseJSON = withObject "styles" $ \o -> do
    styles <- o .: "styles"
    return Styles'{..}

data Foo =
  Foo { _bar :: Float
      , _top :: T.Text
      , _kek :: T.Text
      } deriving (Show, Generic)

makeLenses ''Foo
