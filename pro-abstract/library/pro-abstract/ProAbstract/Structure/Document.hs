module ProAbstract.Structure.Document
    ( Document (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Metadata
import ProAbstract.Structure.Block
import ProAbstract.Structure.HasManyParagraphs
import ProAbstract.Structure.HasManyPlainBlocks
import ProAbstract.Structure.HasManyPlainInlines
import ProAbstract.Tag

data Document ann = Document
    { documentMetadata :: Metadata -- ^ 'ProAbstract.metadata'
    , documentContent :: Blocks ann -- ^ 'ProAbstract.content'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (Document ann) = ann

type instance Content (Document ann) = Blocks ann

type instance Contents (Document ann) = Block ann

instance HasContent (Document ann) (Document ann') where
    content = lens documentContent (\d c -> d { documentContent = c })

instance HasContents (Document ann) (Document ann') where
    contents = content % contents

instance HasMetadata (Document ann) where
    type MetadataOpticKind (Document ann) = A_Lens
    metadata = lens documentMetadata (\d m -> d { documentMetadata = m })

instance HasManyPlainInlines (Document ann) where
    allPlainInlines = content % allPlainInlines

instance HasManyPlainBlocks (Document ann) where
    allPlainBlocks = content % allPlainBlocks

instance HasManyAnnotations (Document ann) (Document ann') where
    allAnnotations = content % allAnnotations

instance HasManyMetadata (Document ann) where
    allMetadata = metadata `adjoin` (content % allMetadata)

instance HasManyParagraphs (Document ann) where
    allParagraphs = content % allParagraphs

instance HasManyTags (Document ann) where
    allTags = content % allTags
    allInlineTags = content % allInlineTags

instance HasWitherableTags (Document ann) where
    witherTags f = traverseOf content (witherTags f)

instance HasManyBlockTags (Document ann) where
    allBlockTags = content % allBlockTags

instance HasWitherableBlockTags (Document ann) where
    witherBlockTags f = traverseOf content (witherBlockTags f)

instance HasWitherableInlineTags (Document ann) where
    witherInlineTags f = traverseOf content (witherInlineTags f)
