module Text.Foldable
  ( module Data.MonoTraversable
  , FunctorString (..)
  , FoldableString (..)
  , TraverseString (..)) where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Char                  as C
import           Data.MonoTraversable
import qualified Data.String                as S
import qualified Data.Text                  as TS
import qualified Data.Text.Encoding         as TSE
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE

import           Text.Isomorphic

{-| MonoFunctor containing Char -}
class FunctorString s where
  map :: (Char -> Char) -> s -> s
  default map :: (MonoFunctor s, Element s ~ Char) => (Char -> Char) -> s -> s
  map = omap

instance FunctorString String
instance FunctorString TS.Text
instance FunctorString TL.Text
instance FunctorString BS.ByteString where map = BSC.map
instance FunctorString BL.ByteString where map = BLC.map

{-| MonoFoldable containing Char -}
class FoldableString s where
  foldMap :: (Char -> s) -> s -> s
  default foldMap :: (MonoFoldable s, Element s ~ Char, Monoid s) => (Char -> s) -> s -> s
  foldMap = ofoldMap
  foldr   :: (Char -> a -> a) -> a -> s -> a
  default foldr :: (MonoFoldable s, Element s ~ Char) => (Char -> a -> a) -> a -> s -> a
  foldr = ofoldr
  foldl'  :: (a -> Char -> a) -> a -> s -> a
  default foldl' :: (MonoFoldable s, Element s ~ Char) => (a -> Char -> a) -> a -> s -> a
  foldl' = ofoldl'
  all     :: (Char -> Bool) -> s -> Bool
  default all :: (MonoFoldable s, Element s ~ Char) => (Char -> Bool) -> s -> Bool
  all = oall
  any     :: (Char -> Bool) -> s -> Bool
  default any :: (MonoFoldable s, Element s ~ Char) => (Char -> Bool) -> s -> Bool
  any = oany
  null    :: s -> Bool
  default null :: (MonoFoldable s) => s -> Bool
  null = onull
  length  :: s -> Int
  default length :: (MonoFoldable s) => s -> Int
  length = olength
  elem    :: Char -> s -> Bool
  default elem :: (MonoFoldable s, Element s ~ Char) => Char -> s -> Bool
  elem = oelem
  maximum :: s -> Char
  default maximum :: (MonoFoldable s, Element s ~ Char) => s -> Char
  maximum = maximumEx
  minimum :: s -> Char
  default minimum :: (MonoFoldable s, Element s ~ Char) => s -> Char
  minimum = minimumEx

instance FoldableString String
instance FoldableString TS.Text
instance FoldableString TL.Text
instance FoldableString BS.ByteString where
  foldMap = BSC.concatMap
  foldr   = BSC.foldr
  foldl'  = BSC.foldl'
  all     = BSC.all
  any     = BSC.any
  elem    = BSC.elem
  maximum = BSC.maximum
  minimum = BSC.minimum
instance FoldableString BL.ByteString where
  foldMap = BLC.concatMap
  foldr   = BLC.foldr
  foldl'  = BLC.foldl'
  all     = BLC.all
  any     = BLC.any
  elem    = BLC.elem
  maximum = BLC.maximum
  minimum = BLC.minimum

{-| MonoTraversable containing Char -}
class (FunctorString s, FoldableString s) => TraverseString s where
  traverse :: Applicative f => (Char -> f Char) -> s -> f s
  default traverse :: (MonoTraversable s, Element s ~ Char, Applicative f) => (Char -> f Char) -> s -> f s
  traverse = otraverse

instance TraverseString String
instance TraverseString TS.Text
instance TraverseString TL.Text
instance TraverseString BS.ByteString where
  traverse f = fmap to . Prelude.traverse f . (to @ BS.ByteString @ String)
instance TraverseString BL.ByteString where
  traverse f = fmap to . Prelude.traverse f . (to @ BL.ByteString @ String)