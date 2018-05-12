{-# LANGUAGE OverloadedStrings,
             DeriveGeneric,
             DeriveAnyClass,
             RecordWildCards,
             TemplateHaskell,
             ScopedTypeVariables #-}
module Types where

import qualified Data.Text    as T
import           GHC.Generics
import           Control.Monad
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens

--

data Style =
  Style { _name :: T.Text
        , _font :: T.Text
        } deriving (Show, Generic)

instance FromJSON Style where
  parseJSON = withObject "style" $ \o -> do
    _name <- o .: "name"
    _font <- o .: "font"
    return Style{..}

makeLenses ''Style

--

data Styles =
  Styles { _styles :: [Style]
         } deriving (Show, Generic)

instance FromJSON Styles where
  parseJSON = withObject "styles" $ \o -> do
    _styles <- o .: "styles"
    return Styles{..}

makeLenses ''Styles
