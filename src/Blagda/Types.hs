{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module Blagda.Types where

import Data.Bifunctor
import Data.Aeson
import GHC.Generics (Generic)

data Post contents meta = Post
  { p_path :: FilePath
  , p_contents :: contents
  , p_meta :: meta
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (ToJSON contents, ToJSON meta) => ToJSON (Post contents meta) where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 2 }

instance Bifunctor Post where
  bimap fab fcd (Post s a c)
    = Post {p_path = s, p_contents = fab a, p_meta = fcd c}

