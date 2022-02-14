module ProAbstract.Tag.TagType
    ( Tag (..)
    )
    where

import ProAbstract.Annotation
import ProAbstract.Metadata

data Tag ann = Tag
    { tagName       :: Text     -- ^ 'ProAbstract.name'
    , tagMetadata   :: Metadata -- ^ 'ProAbstract.metadata'
    , tagAnnotation :: ann      -- ^ 'ProAbstract.annotation'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (Tag ann) = ann

instance HasAnnotation (Tag ann) (Tag ann') where
    annotation = lens tagAnnotation \x a -> x{ tagAnnotation = a }

instance HasMetadata (Tag ann) where
    type MetadataOpticKind (Tag ann) = A_Lens
    metadata = lens tagMetadata (\d m -> d { tagMetadata = m })
