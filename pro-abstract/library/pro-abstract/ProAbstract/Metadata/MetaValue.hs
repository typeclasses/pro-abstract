module ProAbstract.Metadata.MetaValue
    ( MetaValueMaybe (..), MetaValue (..)
    , IsMetaValue (..)
    , isPropertyValue, settingValueMaybe
    , metaValueJust
    ) where

class IsMetaValue a where
    metaValue :: Prism' MetaValueMaybe a
    propertyValue :: a
    settingValue :: Text -> a

instance IsMetaValue MetaValueMaybe where
    metaValue = castOptic simple
    propertyValue = MetaValueMaybe True Nothing
    settingValue s = MetaValueMaybe False (Just s)

instance IsMetaValue MetaValue where
    metaValue = metaValueJust
    propertyValue = MetaValue_Property
    settingValue = MetaValue_Setting

isPropertyValue :: IsMetaValue a => a -> Bool
isPropertyValue = view $ re metaValue % to metaValueMaybe_property

settingValueMaybe :: IsMetaValue a => a -> Maybe Text
settingValueMaybe = view $ re metaValue % to metaValueMaybe_setting

data MetaValueMaybe =
  MetaValueMaybe
    { metaValueMaybe_property :: Bool
    , metaValueMaybe_setting :: Maybe Text
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData, Hashable)

instance Semigroup MetaValueMaybe where
    MetaValueMaybe p1 s1 <> MetaValueMaybe p2 s2 =
        MetaValueMaybe (p1 || p2) (s1 <|> s2)

instance Monoid MetaValueMaybe where
    mempty = MetaValueMaybe False Nothing

data MetaValue =
    MetaValue_Property
  | MetaValue_Setting
        Text -- ^ setting value
  | MetaValue_PropertyAndSetting
        Text -- ^ setting value
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData, Hashable)

instance Semigroup MetaValue where

    x@MetaValue_PropertyAndSetting{} <> _ = x

    MetaValue_Property <> x = forceProperty x
    x <> MetaValue_Property = forceProperty x

    MetaValue_Setting s <> x = forceSetting s x

forceProperty :: MetaValue -> MetaValue
forceProperty = \case
    MetaValue_Setting s -> MetaValue_PropertyAndSetting s
    x -> x

forceSetting :: Text -> MetaValue -> MetaValue
forceSetting s = \case
    MetaValue_Setting _            -> MetaValue_Setting s
    MetaValue_Property             -> MetaValue_PropertyAndSetting s
    MetaValue_PropertyAndSetting _ -> MetaValue_PropertyAndSetting s

metaValueJust :: Prism' MetaValueMaybe MetaValue
metaValueJust = prism' f g
  where
    f = \case
        MetaValue_Property -> MetaValueMaybe True Nothing
        MetaValue_Setting s -> MetaValueMaybe False (Just s)
        MetaValue_PropertyAndSetting s -> MetaValueMaybe True (Just s)
    g = \case
        MetaValueMaybe True  Nothing  -> Just $ MetaValue_Property
        MetaValueMaybe False (Just s) -> Just $ MetaValue_Setting s
        MetaValueMaybe True  (Just s) -> Just $ MetaValue_PropertyAndSetting s
        MetaValueMaybe False Nothing  -> Nothing
