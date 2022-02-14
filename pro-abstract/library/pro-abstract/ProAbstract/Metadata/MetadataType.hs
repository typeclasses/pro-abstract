module ProAbstract.Metadata.MetadataType
    ( Metadata (..)
    ) where

{- | A set of properties and settings, associated with a document or tag.

The namespaces of properties and settings are distinct; a property can share a name with a setting without conflict.
-}
data Metadata = Metadata
    { metadataProperties :: Set Text      -- ^ 'ProAbstract.properties'
    , metadataSettings   :: Map Text Text -- ^ 'ProAbstract.settings'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, Hashable)

instance Monoid Metadata where
    mempty = Metadata mempty mempty

instance Semigroup Metadata where
    Metadata p1 s1 <> Metadata p2 s2 = Metadata (p1 <> p2) (s1 <> s2)
