{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Hedgehog.Internal.Source (
    LineNo(..)
  , ColumnNo(..)
  , Span(..)
  , getCaller

  -- * Re-exports from "GHC.Stack"
  , CallStack
  , HasCallStack
  , callStack
  , withFrozenCallStack
  , guessCabalShareDir
  , ghcVersion
  ) where

import Data.Version (Version(versionBranch))
import System.Directory (getAppUserDataDirectory)
import System.Info (compilerVersion)
import System.FilePath ((</>))

import GHC.Stack (CallStack, HasCallStack, SrcLoc(..))
import GHC.Stack (callStack, getCallStack, withFrozenCallStack)

newtype LineNo =
  LineNo {
      unLineNo :: Int
    } deriving (Eq, Ord, Num, Enum, Real, Integral)

newtype ColumnNo =
  ColumnNo {
      unColumnNo :: Int
    } deriving (Eq, Ord, Num, Enum, Real, Integral)

data Span =
  Span {
      spanPackage :: !String
    , spanFile :: !FilePath
    , spanStartLine :: !LineNo
    , spanStartColumn :: !ColumnNo
    , spanEndLine :: !LineNo
    , spanEndColumn :: !ColumnNo
    } deriving (Eq, Ord)

guessCabalShareDir :: String -> IO FilePath
guessCabalShareDir pkgName = do
  cabalDir <- getAppUserDataDirectory "cabal"

  let
    (x, y, z) = ghcVersion
    ghcVer = show x <> "." <> show y <> "." <> show z
    storeDir = cabalDir </> "store" </> "ghc-" <> ghcVer
    shareDir = storeDir </> pkgName </> "share"

  pure shareDir

ghcVersion :: HasCallStack => (Int, Int, Int)
ghcVersion =
  case versionBranch compilerVersion of
    (x:y:_) -> (x, y, __GLASGOW_HASKELL_PATCHLEVEL1__)
    vb -> error ("Unexpected branch in System.Info.compilerVersion: " <> show vb)

getCaller :: CallStack -> Maybe Span
getCaller stack =
  case getCallStack stack of
    [] ->
      Nothing
    (_, x) : _ ->
      Just $ Span
        (srcLocPackage x)
        (srcLocFile x)
        (fromIntegral $ srcLocStartLine x)
        (fromIntegral $ srcLocStartCol x)
        (fromIntegral $ srcLocEndLine x)
        (fromIntegral $ srcLocEndCol x)

------------------------------------------------------------------------
-- Show instances

instance Show Span where
  showsPrec p (Span pkg file sl sc el ec) =
    showParen (p > 10) $
      showString "Span " .
      showsPrec 11 pkg .
      showChar ' ' .
      showsPrec 11 file .
      showChar ' ' .
      showsPrec 11 sl .
      showChar ' ' .
      showsPrec 11 sc .
      showChar ' ' .
      showsPrec 11 el .
      showChar ' ' .
      showsPrec 11 ec

instance Show LineNo where
  showsPrec p (LineNo x) =
    showParen (p > 10) $
      showString "LineNo " .
      showsPrec 11 x

instance Show ColumnNo where
  showsPrec p (ColumnNo x) =
    showParen (p > 10) $
      showString "ColumnNo " .
      showsPrec 11 x
