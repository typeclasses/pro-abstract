module ProAbstract.Structure.PlainBlock
    ( PlainBlock (..), TaggedPlainBlock (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Metadata
import ProAbstract.Structure.Fragment
import ProAbstract.Tag


-- ⭐ PlainBlock

data PlainBlock ann = PlainBlock
  { plainBlockLines      :: Seq (Fragment ann) -- ^ 'ProAbstract.contentsSeq'
  , plainBlockAnnotation :: ann                -- ^ 'ProAbstract.annotation'
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Contents (PlainBlock ann) = Fragment ann

instance HasContents (PlainBlock ann) (PlainBlock ann) where
    contents = lens plainBlockLines \x a -> x{ plainBlockLines = a }

type instance Annotation (PlainBlock ann) = ann

instance HasAnnotation (PlainBlock ann) (PlainBlock ann) where
    annotation = lens plainBlockAnnotation \x a -> x { plainBlockAnnotation = a }

instance HasManyAnnotations (PlainBlock ann) (PlainBlock ann') where
    allAnnotations = traversalVL \f (PlainBlock t a) ->
        PlainBlock <$> traverseOf (traversed % annotation) f t <*> f a


-- ⭐ TaggedPlainBlock

data TaggedPlainBlock ann =
  TaggedPlainBlock
    { plaintextTag :: Tag ann -- ^ 'ProAbstract.Tag'
    , taggedPlaintext :: PlainBlock ann -- ^ 'ProAbstract.content'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (TaggedPlainBlock ann) = ann

instance HasTag (TaggedPlainBlock ann) where
    type TagOpticKind (TaggedPlainBlock ann) = A_Lens
    tag = lens plaintextTag \x a -> x{ plaintextTag = a }

type instance Content (TaggedPlainBlock ann) = PlainBlock ann

type instance Contents (TaggedPlainBlock ann) = Fragment ann

instance HasContents (TaggedPlainBlock ann) (TaggedPlainBlock ann) where
    contents = content % contents

instance HasManyAnnotations (TaggedPlainBlock ann) (TaggedPlainBlock ann') where
    allAnnotations = traversalVL \f (TaggedPlainBlock t b) -> TaggedPlainBlock
        <$> traverseOf annotation f t <*> traverseOf allAnnotations f b

instance HasAnnotation (TaggedPlainBlock ann) (TaggedPlainBlock ann) where
     annotation = tag % annotation

instance HasContent (TaggedPlainBlock ann) (TaggedPlainBlock ann) where
    content = lens taggedPlaintext \x c -> x{ taggedPlaintext = c }

instance HasMetadata (TaggedPlainBlock ann) where
    type MetadataOpticKind (TaggedPlainBlock ann) = A_Lens
    metadata = tag % metadata
