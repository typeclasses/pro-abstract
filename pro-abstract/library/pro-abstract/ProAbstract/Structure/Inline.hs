module ProAbstract.Structure.Inline
    ( Inline (..), Line (..), Lines (..), TaggedLines (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Metadata
import ProAbstract.Structure.CanBePlain
import ProAbstract.Structure.CanFork
import ProAbstract.Structure.Fork
import ProAbstract.Structure.Fragment
import ProAbstract.Structure.HasManyPlainInlines
import ProAbstract.Structure.Plain
import ProAbstract.Tag


-- ⭐ Inline

data Inline ann =
    InlineFork (TaggedLines ann) -- ^ 'ProAbstract.fork'
  | InlinePlain (Fragment ann) -- ^ 'ProAbstract.plain'
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (Inline ann) = ann

type instance Fork (Inline ann) = TaggedLines ann

type instance Plain (Inline ann) = Fragment ann

instance HasMetadata (Inline ann) where
    type MetadataOpticKind (Inline ann) = An_AffineTraversal
    metadata = tag % metadata

instance HasManyAnnotations (Inline ann) (Inline ann') where
    allAnnotations = traversalVL \f -> \case
        InlinePlain x -> InlinePlain <$> traverseOf annotation f x
        InlineFork  x -> InlineFork  <$> traverseOf allAnnotations f x

instance HasAnnotation (Inline ann) (Inline ann) where
    annotation = lens f g
      where
        f = \case
            InlineFork  x -> view annotation x
            InlinePlain x -> view annotation x
        g = \case
            InlineFork  x -> \a -> InlineFork  (set annotation a x)
            InlinePlain x -> \a -> InlinePlain (set annotation a x)

instance HasManyPlainInlines (Inline ann) where
    allPlainInlines = plain `adjoin` (fork % allPlainInlines)

instance HasTag (Inline ann) where
    type TagOpticKind (Inline ann) = An_AffineTraversal
    tag = atraversal f g
      where
        f = \case
            InlineFork x -> Right (view tag x)
            x -> Left x
        g = \case
            InlineFork x -> \a -> InlineFork (set tag a x)
            x -> \_ -> x

instance HasManyTags (Inline ann) where
    allTags = fork % allTags
    allInlineTags = allTags

instance HasManyMetadata (Inline ann) where
    allMetadata = fork % allMetadata

instance CanFork (Inline ann) where
    fork = prism' InlineFork \case{ InlineFork t -> Just t; _ -> Nothing }

instance CanBePlain (Inline ann) where
    plain = prism' InlinePlain \case{ InlinePlain t -> Just t; _ -> Nothing }


-- ⭐ Line

data Line ann = Line
    { lineInlines :: Seq (Inline ann) -- ^ 'ProAbstract.contentsSeq'
    , lineAnnotation :: ann -- ^ 'ProAbstract.annotation'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

instance IsList (Line ()) where
    type Item (Line ()) = Inline ()
    toList (Line xs ()) = toList xs
    fromList xs = Line (fromList xs) ()

type instance Contents (Line ann) = Inline ann

instance HasContents (Line ann) (Line ann) where
    contents = lens lineInlines \x a -> x{ lineInlines = a }

type instance Annotation (Line ann) = ann

instance HasAnnotation (Line ann) (Line ann) where
    annotation = lens lineAnnotation \x a -> x{ lineAnnotation = a }

instance HasManyPlainInlines (Line ann) where
    allPlainInlines = contents % traversed % allPlainInlines

instance HasManyAnnotations (Line ann) (Line ann') where
    allAnnotations = traversalVL \f (Line xs a) ->
        Line <$> traverseOf (traversed % allAnnotations) f xs <*> f a

instance HasManyMetadata (Line ann) where
    allMetadata = contents % traversed % allMetadata

instance HasManyTags (Line ann) where
    allTags = contents % traversed % allTags
    allInlineTags = allTags

instance HasWitherableInlineTags (Line ann) where
    witherInlineTags = witherTags

instance HasWitherableTags (Line ann) where
    witherTags f = traverseOf contents $
        seqWither \case
            InlinePlain x -> pure . Just . InlinePlain $ x
            InlineFork x -> f (view tag x) >>= \case
                Nothing -> pure Nothing
                Just t -> Just . InlineFork . set tag t <$> traverseOf content (witherTags f) x


-- ⭐ Lines

newtype Lines ann =
  Lines
    (Seq (Line ann)) -- ^ 'ProAbstract.contentsSeq'
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)
  deriving newtype (Semigroup, Monoid)

instance IsList (Lines ann) where
    type Item (Lines ann) = Line ann
    toList (Lines xs) = toList xs
    fromList xs = Lines (fromList xs)

type instance Contents (Lines ann) = Line ann

instance HasContents (Lines ann) (Lines ann') where
    contents = castOptic coerced

type instance Annotation (Lines ann) = ann

instance HasManyPlainInlines (Lines ann) where
    allPlainInlines = contents % traversed % allPlainInlines

instance HasManyAnnotations (Lines ann) (Lines ann') where
    allAnnotations = contents % traversed % allAnnotations

instance HasManyMetadata (Lines ann) where
    allMetadata = contents % traversed % allMetadata

instance HasManyTags (Lines ann) where
    allTags = contents % traversed % allTags
    allInlineTags = allTags

instance HasWitherableInlineTags (Lines ann) where
    witherInlineTags = witherTags

instance HasWitherableTags (Lines ann) where
    witherTags f = traverseOf (contents % traversed) (witherTags f)


-- ⭐ TaggedLines

data TaggedLines ann =
  TaggedLines
    { linesTag :: Tag ann -- ^ 'ProAbstract.tag'
    , taggedLines :: Lines ann -- ^ 'ProAbstract.content'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (TaggedLines ann) = ann

instance HasTag (TaggedLines ann) where
    type TagOpticKind (TaggedLines ann) = A_Lens
    tag = lens linesTag \x a -> x{ linesTag = a }

type instance Content (TaggedLines ann) = Lines ann

type instance Contents (TaggedLines ann) = Line ann

instance HasManyAnnotations (TaggedLines ann) (TaggedLines ann') where
    allAnnotations = traversalVL \f (TaggedLines t b) -> TaggedLines
        <$> traverseOf annotation f t <*> traverseOf allAnnotations f b

instance HasAnnotation (TaggedLines ann) (TaggedLines ann) where
     annotation = tag % annotation

instance HasContent (TaggedLines ann) (TaggedLines ann) where
    content = lens taggedLines \x c -> x{ taggedLines = c }

instance HasContents (TaggedLines ann) (TaggedLines ann) where
    contents = content % contents

instance HasMetadata (TaggedLines ann) where
    type MetadataOpticKind (TaggedLines ann) = A_Lens
    metadata = tag % metadata

instance HasManyPlainInlines (TaggedLines ann) where
    allPlainInlines = content % allPlainInlines

instance HasManyTags (TaggedLines ann) where
    allTags = tag `adjoin` (content % allTags)
    allInlineTags = allTags

instance HasWitherableInlineTags (TaggedLines ann) where
    witherInlineTags f = traverseOf content (witherInlineTags f)

instance HasManyMetadata (TaggedLines ann) where
   allMetadata = allTags % metadata
