{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Blagda.Agda where



import           Agda.Interaction.FindFile (SourceFile(..))
import           Agda.Interaction.Imports
import           Agda.Interaction.Options
import           Agda.Syntax.Abstract
import           Agda.Syntax.Abstract.Views
import           Agda.Syntax.Common
import qualified Agda.Syntax.Concrete as Con
import           Agda.Syntax.Info
import           Agda.Syntax.Internal (Type, Dom, domName)
import           Agda.Syntax.Position
import           Agda.Syntax.Scope.Base
import           Agda.Syntax.Translation.AbstractToConcrete (abstractToConcrete_)
import           Agda.Syntax.Translation.InternalToAbstract ( Reify(reify) )
import           Agda.TypeChecking.Monad
import           Agda.Utils.FileName
import qualified Agda.Utils.Maybe.Strict as S
import           Agda.Utils.Pretty
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Generics
import           Data.List
import           Data.List.NonEmpty (NonEmpty((:|)))
import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Traversable
import           Development.Shake.FilePath

moduleName :: FilePath -> String
moduleName = intercalate "." . splitDirectories

fakePath :: QName -> Maybe FilePath
fakePath (QName (MName xs) _) =
  listToMaybe
    [ l <.> "html"
    | l <- map (intercalate ".") (inits (map (render . pretty . nameConcrete) xs))
    , l `elem` builtinModules
    ]

removeImpls :: Expr -> Expr
removeImpls (Pi _ (x :| xs) e) =
  makePi (map (mapExpr removeImpls) $ filter ((/= Hidden) . getHiding) (x:xs)) (removeImpls e)
removeImpls (Fun span arg ret) =
  Fun span (removeImpls <$> arg) (removeImpls ret)
removeImpls e = e

runAgda :: (String -> TCMT IO a) -> IO a
runAgda k = do
  e <- runTCMTop $ do
    p <- setupTCM
    k p
  case e of
    Left s -> error (show s)
    Right x -> pure x

setupTCM :: TCMT IO String
setupTCM = do
  absp <- liftIO $ absolute "./site"
  setCommandLineOptions' absp defaultOptions{optLocalInterfaces = True}
  pure (filePath absp)

killDomainNames :: Type -> Type
killDomainNames = everywhere (mkT unDomName) where
  unDomName :: Dom Type -> Dom Type
  unDomName m = m{ domName = Nothing }

killQual :: Con.Expr -> Con.Expr
killQual = everywhere (mkT unQual) where
  unQual :: Con.QName -> Con.QName
  unQual (Con.Qual _ x) = unQual x
  unQual x = x

tcAndLoadPublicNames :: FilePath -> String -> TCMT IO (Map Text Text)
tcAndLoadPublicNames path basepn = do
  source <- parseSource . SourceFile =<< liftIO (absolute path)
  cr <- typeCheckMain TypeCheck source

  let iface = crInterface cr

  setScope (iInsideScope iface)
  scope <- getScope

  li <- fmap catMaybes . for (toList (_scopeInScope scope)) $ \name -> do
    t <- getConstInfo' name
    case t of
      Left _ -> pure Nothing
      Right d -> do
        expr <- reify . killDomainNames $ defType d
        t <- fmap (render . pretty . killQual) .
          abstractToConcrete_ . removeImpls $ expr

        case rangeFile (nameBindingSite (qnameName name)) of
          S.Just (filePath -> f)
            | ("Agda/Builtin" `isInfixOf` f) || ("Agda/Primitive" `isInfixOf` f) ->
              pure $ do
                fp <- fakePath name
                pure (name, fp, t)
            | otherwise -> do
              let
                f' = moduleName $ dropExtensions (makeRelative basepn f)
                modMatches = f' `isPrefixOf` render (pretty name)

              pure $ do
                unless modMatches Nothing
                pure (name, f' <.> "html", t)
          S.Nothing -> pure Nothing

  let
    f (name, modn, ty) =
      case rStart (nameBindingSite (qnameName name)) of
        Just pn -> pure (Text.pack (modn <> "#" <> show (posPos pn)), Text.pack ty)
        Nothing -> Nothing

  pure (Map.fromList (mapMaybe f li))

makePi :: [TypedBinding] -> Expr -> Expr
makePi [] = id
makePi (b:bs) = Pi exprNoRange (b :| bs)

builtinModules :: [String]
builtinModules =
  [ "Agda.Builtin.Bool"
  , "Agda.Builtin.Char"
  -- , "Agda.Builtin.Cubical.HCompU"
  -- , "Agda.Builtin.Cubical.Path"
  -- , "Agda.Builtin.Cubical.Sub"
  , "Agda.Builtin.Float"
  , "Agda.Builtin.FromNat"
  , "Agda.Builtin.FromNeg"
  , "Agda.Builtin.Int"
  , "Agda.Builtin.List"
  , "Agda.Builtin.Maybe"
  , "Agda.Builtin.Nat"
  , "Agda.Builtin.Reflection"
  , "Agda.Builtin.Sigma"
  , "Agda.Builtin.String"
  , "Agda.Builtin.Unit"
  , "Agda.Builtin.Word"
  , "Agda.Primitive.Cubical"
  , "Agda.Primitive"
  ]

