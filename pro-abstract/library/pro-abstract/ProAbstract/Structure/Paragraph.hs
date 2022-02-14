module ProAbstract.Structure.Paragraph
    ( Paragraph (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Metadata
import ProAbstract.Structure.HasManyPlainInlines
import ProAbstract.Structure.Inline
import ProAbstract.Tag

-- | A collection of 'Lines'. A 'Paragraph' represents the border between block and inline contexts. All ancestors of a paragraph are block items or a document, and all children are inline items.
data Paragraph ann = Paragraph
    { paragraphContent :: Lines ann -- ^ 'ProAbstract.content'
    , paragraphAnnotation :: ann -- ^ 'ProAbstract.annotation'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Content (Paragraph ann) = Lines ann

type instance Contents (Paragraph ann) = Line ann

type instance Annotation (Paragraph ann) = ann

instance HasManyAnnotations (Paragraph ann) (Paragraph ann') where
    allAnnotations = traversalVL \f (Paragraph c a) ->
        Paragraph <$> traverseOf allAnnotations f c <*> f a

instance HasAnnotation (Paragraph ann) (Paragraph ann) where
    annotation = lens paragraphAnnotation \x a -> x{ paragraphAnnotation = a }

instance HasContent (Paragraph ann) (Paragraph ann) where
    content = lens paragraphContent (\t c -> t { paragraphContent = c })

instance HasContents (Paragraph ann) (Paragraph ann) where
    contents = content % contents

instance HasManyPlainInlines (Paragraph ann) where
    allPlainInlines = content % allPlainInlines

instance HasManyMetadata (Paragraph ann) where
    allMetadata = content % allMetadata

instance HasManyTags (Paragraph ann) where
    allTags = content % allTags
    allInlineTags = content % allInlineTags

instance HasWitherableInlineTags (Paragraph ann) where
    witherInlineTags f = traverseOf content (witherInlineTags f)
