{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Blagda.Latex where

import qualified Data.ByteString.Lazy as LazyBS
import           Data.Generics
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Forward (cacheAction)


newtype LatexEquation = LatexEquation (Bool, Text) -- TODO: Less lazy instance
  deriving newtype (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult LatexEquation = Text


buildLatexEqn :: Bool -> Text -> Action Text
buildLatexEqn display tex = cacheAction (LatexEquation (display, tex)) $ do
  need [".macros"]

  let args = ["-f", ".macros", "-t"] ++ ["-d" | display]
      stdin = LazyBS.fromStrict $ Text.encodeUtf8 tex
  Stdout out <- command [StdinBS stdin] "katex" args
  pure . Text.stripEnd . Text.decodeUtf8 $ out

