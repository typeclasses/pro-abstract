module ProAbstract.Metadata
  ( Metadata (..), HasMetadata (..), HasManyMetadata (..)
  , properties, settings, hasProperty, atSetting
  , MetaItem (..), MetaValue (..), metaMap, metaList
  ) where

import ProAbstract.Metadata.HasManyMetadata
import ProAbstract.Metadata.HasMetadata
import ProAbstract.Metadata.HasMetadataOptics
import ProAbstract.Metadata.MetaItem
import ProAbstract.Metadata.MetaValue
import ProAbstract.Metadata.MetadataType
