module ProAbstract.Metadata.HasMetadata
    ( HasMetadata (..), Metadata
    ) where

import ProAbstract.Metadata.MetadataType (Metadata)

class HasMetadata x where
    type MetadataOpticKind x
    metadata :: Optic' (MetadataOpticKind x) NoIx x Metadata

instance HasMetadata Metadata where
    type MetadataOpticKind Metadata = An_Iso
    metadata = castOptic simple
