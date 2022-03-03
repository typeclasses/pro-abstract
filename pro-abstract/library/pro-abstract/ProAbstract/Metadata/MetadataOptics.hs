module ProAbstract.Metadata.MetadataOptics where

import ProAbstract.Metadata.MetaValue
import ProAbstract.Metadata.MetadataType

-- | Targets all properties from metadata.
properties :: Lens' Metadata (Set Text)
properties = lens metadataProperties (\m p -> m { metadataProperties = p })

-- | Targets all settings from metadata.
settings :: Lens' Metadata (Map Text Text)
settings = lens metadataSettings (\m s -> m { metadataSettings = s })

-- | Check if metadata includes a property. Using this optic as a setter will add a property if set to 'True' and remove the property when set to 'False'.
hasProperty :: Text -> Lens' Metadata Bool
hasProperty k = properties % lens (setMember k) (\s b -> (if b then setInsert else setDelete) k s)

-- | Targets a setting from metadata. Returns 'Nothing' if no value is set.
atSetting :: Text -> Lens' Metadata (Maybe Text)
atSetting k = settings % lens (mapLookup k) (\m x -> maybe (mapDelete k) (mapInsert k) x m)

metaMap :: Iso' Metadata (Map Text MetaValue)
metaMap = iso f g
  where
    f x = mapUnionWith (<>) ps ss
      where
        ps = mapFromSet (const propertyValue) (metadataProperties x)
        ss = fmap settingValue (metadataSettings x)
    g x = Metadata ps ss
      where
        ps = mapKeysSet $ mapFilter isPropertyValue x
        ss = mapMapMaybe settingValueMaybe x
