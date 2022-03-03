module Containers where

import BasePrelude (Applicative, Bool, Either, Foldable, Maybe (..), Monoid, Ord, Semigroup, maybe, pure, (.), (<*>), (<>))
import Data.Foldable (fold, toList)
import Data.Function (fix, id)
import Optics.Core (Prism', prism')

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as Text

mapLookup :: Ord k => k -> Map k a -> Maybe a
mapLookup = Map.lookup

mapDelete :: Ord k => k -> Map k a -> Map k a
mapDelete = Map.delete

mapInsert :: Ord k => k -> a -> Map k a -> Map k a
mapInsert = Map.insert

mapFromSet :: (k -> a) -> Set k -> Map k a
mapFromSet = Map.fromSet

mapMapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapMapEither = Map.mapEither

mapMapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMapMaybe = Map.mapMaybe

mapKeysSet :: Map k a -> Set k
mapKeysSet = Map.keysSet

mapUnionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
mapUnionWith = Map.unionWith

mapFilter :: (a -> Bool) -> Map k a -> Map k a
mapFilter = Map.filter

mapToList :: Map k a -> [(k, a)]
mapToList = Map.toAscList

mapFoldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m
mapFoldMapWithKey = Map.foldMapWithKey

setMember :: Ord a => a -> Set a -> Bool
setMember = Set.member

setInsert :: Ord a => a -> Set a -> Set a
setInsert = Set.insert

setDelete :: Ord a => a -> Set a -> Set a
setDelete = Set.delete

seqWither :: Applicative f => (a -> f (Maybe b)) -> Seq a -> f (Seq b)
seqWither f = fix \r -> seqReduceR (pure Seq.Empty) \x xs -> pure seqConsMaybe <*> f x <*> r xs

seqReduceR :: b -> (a -> Seq a -> b) -> Seq a -> b
seqReduceR z f = \case{ Seq.Empty -> z; (Seq.:<|) x xs -> f x xs }

seqConsMaybe :: Maybe a -> Seq a -> Seq a
seqConsMaybe = maybe id (Seq.:<|)

seqFromMaybe :: Maybe a -> Seq a
seqFromMaybe = maybe Seq.empty Seq.singleton

seqSingleton :: Prism' (Seq a) a
seqSingleton = prism' Seq.singleton f
  where
    f = \case
        (x Seq.:<| Seq.Empty) -> Just x
        _ -> Nothing

seqAtMostOne :: Prism' (Seq a) (Maybe a)
seqAtMostOne = prism' (maybe Seq.empty Seq.singleton) f
  where
    f = \case
        Seq.Empty -> Just Nothing
        (x Seq.:<| Seq.Empty) -> Just (Just x)
        _ -> Nothing

seqToList :: Seq a -> [a]
seqToList = toList

seqConcat :: Seq (Seq a) -> Seq a
seqConcat = fold

textEmpty :: Text.Text
textEmpty = Text.empty

textConcat :: Foldable t => t Text.Text -> Text.Text
textConcat = Text.concat . toList

seqAppendAndConcat :: Semigroup a => Seq a -> Seq a -> Seq a
seqAppendAndConcat x Empty = x
seqAppendAndConcat Empty x = x
seqAppendAndConcat (xs :|> x) (y :<| ys) = xs <> Seq.singleton (x <> y) <> ys

seqAppendAndConcatWith :: (a -> a -> Maybe a) -> Seq a -> Seq a -> Seq a
seqAppendAndConcatWith _ x Empty = x
seqAppendAndConcatWith _ Empty x = x
seqAppendAndConcatWith f a@(xs :|> x) b@(y :<| ys) =
    case f x y of
        Nothing -> a <> b
        Just z -> xs <> Seq.singleton z <> ys
