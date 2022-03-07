module ProAbstract.Structure.Inline
    ( Inline (..), Line (..), Lines (..), TaggedLines (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Metadata
import ProAbstract.Sequence.SequenceFamily
import ProAbstract.Structure.CanBePlain
import ProAbstract.Structure.CanFork
import ProAbstract.Structure.Fork
import ProAbstract.Structure.Fragment
import ProAbstract.Structure.HasManyPlainInlines
import ProAbstract.Structure.Plain
import ProAbstract.Tag


-- ⭐ Inline

data Inline seq ann =
    InlineFork (TaggedLines seq ann) -- ^ 'ProAbstract.fork'
  | InlinePlain (Fragment ann) -- ^ 'ProAbstract.plain'
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (Inline seq ann) = ann

type instance Fork (Inline seq ann) = TaggedLines seq ann

type instance Plain (Inline seq ann) = Fragment ann

instance HasMetadata (Inline seq ann) where
    type MetadataOpticKind (Inline seq ann) = An_AffineTraversal
    metadata = tag % metadata

instance HasManyAnnotations (Inline seq ann) (Inline seq ann') where
    allAnnotations = traversalVL \f -> \case
        InlinePlain x -> InlinePlain <$> traverseOf annotation f x
        InlineFork  x -> InlineFork  <$> traverseOf allAnnotations f x

instance HasAnnotation (Inline seq ann) (Inline seq ann) where
    annotation = lens f g
      where
        f = \case
            InlineFork  x -> view annotation x
            InlinePlain x -> view annotation x
        g = \case
            InlineFork  x -> \a -> InlineFork  (set annotation a x)
            InlinePlain x -> \a -> InlinePlain (set annotation a x)

instance HasManyPlainInlines (Inline seq ann) where
    allPlainInlines = plain `adjoin` (fork % allPlainInlines)

instance HasTag (Inline seq ann) where
    type TagOpticKind (Inline seq ann) = An_AffineTraversal
    tag = atraversal f g
      where
        f = \case
            InlineFork x -> Right (view tag x)
            x -> Left x
        g = \case
            InlineFork x -> \a -> InlineFork (set tag a x)
            x -> \_ -> x

instance HasManyTags (Inline seq ann) where
    allTags = fork % allTags
    allInlineTags = allTags

instance HasManyMetadata (Inline seq ann) where
    allMetadata = fork % allMetadata

instance CanFork (Inline seq ann) where
    fork = prism' InlineFork \case{ InlineFork t -> Just t; _ -> Nothing }

instance CanBePlain (Inline seq ann) where
    plain = prism' InlinePlain \case{ InlinePlain t -> Just t; _ -> Nothing }


-- ⭐ Line

data Line seq ann = Line
    { lineInlines :: seq (Inline seq ann) -- ^ 'ProAbstract.contentsSeq'
    , lineAnnotation :: ann -- ^ 'ProAbstract.annotation'
    }

deriving stock instance
    Generic (Line seq ann)

deriving stock instance
    ( Eq ann
    , forall a. Eq a => Eq (seq a)
    ) =>
    Eq (Line seq ann)

deriving stock instance
    ( Show ann
    , forall a. Show a => Show (seq a)
    ) =>
    Show (Line seq ann)

deriving anyclass instance
    ( Hashable ann
    , forall a. Eq a => Eq (seq a)
    , forall a. Hashable a => Hashable (seq a)
    ) =>
    Hashable (Line seq ann)

deriving anyclass instance
    ( NFData ann
    , forall a. NFData a => NFData (seq a)
    ) =>
    NFData (Line seq ann)

instance
    ( forall a. IsList (seq a)
    , Item (seq (Inline seq ())) ~ Inline seq ()
    ) =>
  IsList (Line seq ()) where
    type Item (Line seq ()) = Inline seq ()
    toList (Line xs ()) = toList xs
    fromList xs = Line (fromList xs) ()

type instance Sequence (Line seq ann) = seq

type instance Contents (Line seq ann) = Inline seq ann

instance HasContents (Line seq ann) (Line seq ann) where
    contents = lens lineInlines \x a -> x{ lineInlines = a }

type instance Annotation (Line seq ann) = ann

instance HasAnnotation (Line seq ann) (Line seq ann) where
    annotation = lens lineAnnotation \x a -> x{ lineAnnotation = a }

instance Traversable seq => HasManyPlainInlines (Line seq ann) where
    allPlainInlines = contents % traversed % allPlainInlines

instance Traversable seq => HasManyAnnotations (Line seq ann) (Line seq ann') where
    allAnnotations = traversalVL \f (Line xs a) ->
        Line <$> traverseOf (traversed % allAnnotations) f xs <*> f a

instance Traversable seq => HasManyMetadata (Line seq ann) where
    allMetadata = contents % traversed % allMetadata

instance Traversable seq => HasManyTags (Line seq ann) where
    allTags = contents % traversed % allTags
    allInlineTags = allTags

instance HasWitherableInlineTags (Line Seq ann) where
    witherInlineTags = witherTags

instance HasWitherableTags (Line Seq ann) where
    witherTags f = traverseOf contents $
        seqWither \case
            InlinePlain x -> pure . Just . InlinePlain $ x
            InlineFork x -> f (view tag x) >>= \case
                Nothing -> pure Nothing
                Just t -> Just . InlineFork . set tag t <$> traverseOf content (witherTags f) x


-- ⭐ Lines

newtype Lines seq ann =
  Lines
    (seq (Line seq ann)) -- ^ 'ProAbstract.contentsSeq'
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)
  deriving newtype (Semigroup, Monoid)

instance IsList (Lines seq ann) where
    type Item (Lines seq ann) = Line seq ann
    toList (Lines xs) = toList xs
    fromList xs = Lines (fromList xs)

type instance Contents (Lines seq ann) = Line seq ann

instance HasContents (Lines seq ann) (Lines seq ann') where
    contents = castOptic coerced

type instance Annotation (Lines seq ann) = ann

instance HasManyPlainInlines (Lines seq ann) where
    allPlainInlines = contents % traversed % allPlainInlines

instance HasManyAnnotations (Lines seq ann) (Lines seq ann') where
    allAnnotations = contents % traversed % allAnnotations

instance HasManyMetadata (Lines seq ann) where
    allMetadata = contents % traversed % allMetadata

instance HasManyTags (Lines seq ann) where
    allTags = contents % traversed % allTags
    allInlineTags = allTags

instance HasWitherableInlineTags (Lines seq ann) where
    witherInlineTags = witherTags

instance HasWitherableTags (Lines seq ann) where
    witherTags f = traverseOf (contents % traversed) (witherTags f)


-- ⭐ TaggedLines

data TaggedLines seq ann =
  TaggedLines
    { linesTag :: Tag ann -- ^ 'ProAbstract.tag'
    , taggedLines :: Lines seq ann -- ^ 'ProAbstract.content'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (TaggedLines seq ann) = ann

instance HasTag (TaggedLines seq ann) where
    type TagOpticKind (TaggedLines seq ann) = A_Lens
    tag = lens linesTag \x a -> x{ linesTag = a }

type instance Content (TaggedLines seq ann) = Lines seq ann

type instance Contents (TaggedLines seq ann) = Line seq ann

instance HasManyAnnotations (TaggedLines seq ann) (TaggedLines seq ann') where
    allAnnotations = traversalVL \f (TaggedLines t b) -> TaggedLines
        <$> traverseOf annotation f t <*> traverseOf allAnnotations f b

instance HasAnnotation (TaggedLines seq ann) (TaggedLines seq ann) where
     annotation = tag % annotation

instance HasContent (TaggedLines seq ann) (TaggedLines seq ann) where
    content = lens taggedLines \x c -> x{ taggedLines = c }

instance HasContents (TaggedLines seq ann) (TaggedLines seq ann) where
    contents = content % contents

instance HasMetadata (TaggedLines seq ann) where
    type MetadataOpticKind (TaggedLines seq ann) = A_Lens
    metadata = tag % metadata

instance HasManyPlainInlines (TaggedLines seq ann) where
    allPlainInlines = content % allPlainInlines

instance HasManyTags (TaggedLines seq ann) where
    allTags = tag `adjoin` (content % allTags)
    allInlineTags = allTags

instance HasWitherableInlineTags (TaggedLines seq ann) where
    witherInlineTags f = traverseOf content (witherInlineTags f)

instance HasManyMetadata (TaggedLines seq ann) where
   allMetadata = allTags % metadata
