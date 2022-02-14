module ProAbstract.Tag.HasManyTags
    ( HasManyTags (..), HasWitherableTags (..)
    , HasManyBlockTags (..), HasWitherableBlockTags (..)
    , HasWitherableInlineTags (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Tag.TagType

class HasManyTags x where
    allTags :: Traversal' x (Tag (Annotation x))
    allInlineTags :: Traversal' x (Tag (Annotation x))

class HasWitherableTags x where
    witherTags :: Monad f => (Tag (Annotation x) -> f (Maybe (Tag (Annotation x)))) -> x -> f x

    mapMaybeTags :: (Tag (Annotation x) -> Maybe (Tag (Annotation x))) -> x -> x
    mapMaybeTags f = runIdentity . witherTags (Identity . f)

class HasManyTags x => HasManyBlockTags x where
    allBlockTags :: Traversal' x (Tag (Annotation x))

class HasManyBlockTags x => HasWitherableBlockTags x where
    witherBlockTags :: Monad f => (Tag (Annotation x) -> f (Maybe (Tag (Annotation x)))) -> x -> f x

    mapMaybeBlockTags :: (Tag (Annotation x) -> Maybe (Tag (Annotation x))) -> x -> x
    mapMaybeBlockTags f = runIdentity . witherBlockTags (Identity . f)

class HasManyTags x => HasWitherableInlineTags x where
    witherInlineTags :: Monad f => (Tag (Annotation x) -> f (Maybe (Tag (Annotation x)))) -> x -> f x

    mapMaybeInlineTags :: (Tag (Annotation x) -> Maybe (Tag (Annotation x))) -> x -> x
    mapMaybeInlineTags f = runIdentity . witherInlineTags (Identity . f)
