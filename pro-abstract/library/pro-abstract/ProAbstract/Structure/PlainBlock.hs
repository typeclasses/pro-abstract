module ProAbstract.Structure.PlainBlock
    ( PlainBlock (..), Tagged (..)
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


-- ⭐ Tagged PlainBlock

data instance Tagged (PlainBlock ann) =
  TaggedPlainBlock
    { plaintextTag :: Tag ann -- ^ 'ProAbstract.Tag'
    , taggedPlaintext :: PlainBlock ann -- ^ 'ProAbstract.content'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (Tagged (PlainBlock ann)) = ann

instance HasTag (Tagged (PlainBlock ann)) where
    type TagOpticKind (Tagged (PlainBlock ann)) = A_Lens
    tag = lens plaintextTag \x a -> x{ plaintextTag = a }

type instance Content (Tagged (PlainBlock ann)) = PlainBlock ann

type instance Contents (Tagged (PlainBlock ann)) = Fragment ann

instance HasContents (Tagged (PlainBlock ann)) (Tagged (PlainBlock ann)) where
    contents = content % contents

instance HasManyAnnotations (Tagged (PlainBlock ann)) (Tagged (PlainBlock ann')) where
    allAnnotations = traversalVL \f (TaggedPlainBlock t b) -> TaggedPlainBlock
        <$> traverseOf annotation f t <*> traverseOf allAnnotations f b

instance HasAnnotation (Tagged (PlainBlock ann)) (Tagged (PlainBlock ann)) where
     annotation = tag % annotation

instance HasContent (Tagged (PlainBlock ann)) (Tagged (PlainBlock ann)) where
    content = lens taggedPlaintext \x c -> x{ taggedPlaintext = c }

instance HasMetadata (Tagged (PlainBlock ann)) where
    type MetadataOpticKind (Tagged (PlainBlock ann)) = A_Lens
    metadata = tag % metadata
