{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Blagda.Types where

import Text.Pandoc (Pandoc)

data Post a = Post
  { p_path :: FilePath
  , p_contents :: Pandoc
  , p_meta :: a
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

