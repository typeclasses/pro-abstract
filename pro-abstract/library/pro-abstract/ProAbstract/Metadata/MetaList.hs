module ProAbstract.Metadata.MetaList
    ( metaList
    ) where

import ProAbstract.Metadata.MetaItem
import ProAbstract.Metadata.MetaValue
import ProAbstract.Metadata.MetadataOptics
import ProAbstract.Metadata.MetadataType

metaList :: Getter Metadata [MetaItem]
metaList = metaMap % to (mapFoldMapWithKey metaValueList)

metaValueList :: Text -> MetaValue -> [MetaItem]
metaValueList k = \case
    MetaValue_Property -> [ Property k ]
    MetaValue_Setting v -> [ Setting k v ]
    MetaValue_PropertyAndSetting v -> [ Property k, Setting k v ]
