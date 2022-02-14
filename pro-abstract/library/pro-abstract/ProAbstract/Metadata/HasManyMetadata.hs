module ProAbstract.Metadata.HasManyMetadata
    ( HasManyMetadata (..)
    ) where

import ProAbstract.Metadata.MetadataType

class HasManyMetadata x where
    allMetadata :: Traversal' x Metadata
