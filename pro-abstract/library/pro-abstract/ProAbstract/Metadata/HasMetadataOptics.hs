module ProAbstract.Metadata.HasMetadataOptics where

import ProAbstract.Metadata.HasMetadata
import ProAbstract.Metadata.MetaItem
import ProAbstract.Metadata.MetaValue

import qualified ProAbstract.Metadata.MetaList as Meta
import qualified ProAbstract.Metadata.MetadataOptics as Meta

-- | Fetch all properties from items which contain metadata.
properties :: (HasMetadata m, JoinKinds (MetadataOpticKind m) A_Lens k) => Optic' k NoIx m (Set Text)
properties = metadata % Meta.properties

-- | Fetch all settings defined on items which contain metadata.
settings :: (HasMetadata m, JoinKinds (MetadataOpticKind m) A_Lens k) => Optic' k NoIx m (Map Text Text)
settings = metadata % Meta.settings

-- | Check if a property is attached to an item with metadata. Using this optic as a setter will add a property if set to 'True' and remove the property when set to 'False'.
hasProperty :: (HasMetadata m, JoinKinds (MetadataOpticKind m) A_Lens k) => Text -> Optic' k NoIx m Bool
hasProperty k = metadata % Meta.hasProperty k

-- | Select a setting from an item attached to metadata. Returns 'Nothing' if no value is set.
atSetting :: (HasMetadata m, JoinKinds (MetadataOpticKind m) A_Lens k) => Text -> Optic' k NoIx m (Maybe Text)
atSetting k = metadata % Meta.atSetting k

metaMap :: (HasMetadata m, JoinKinds (MetadataOpticKind m) An_Iso k) => Optic' k NoIx m (Map Text MetaValue)
metaMap = metadata % Meta.metaMap

metaList :: (HasMetadata m, Is (MetadataOpticKind m) An_AffineFold) => Getter m [MetaItem]
metaList = to $ maybe [] (view Meta.metaList) . preview metadata
