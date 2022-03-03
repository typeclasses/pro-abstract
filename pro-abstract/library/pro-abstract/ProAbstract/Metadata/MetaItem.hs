module ProAbstract.Metadata.MetaItem where

data MetaItem =
    Property
        Text -- ^ key
  | Setting
        Text -- ^ key
        Text -- ^ value
    deriving stock (Show, Generic)
    deriving anyclass (NFData, Hashable)

instance Ord MetaItem where
    compare = compare `on` \case
        Property x -> (x, Nothing)
        Setting x y -> (x, Just y)

instance Eq MetaItem where
    a == b = EQ == compare a b

metaKeyValue :: Iso' MetaItem (Text, Maybe Text)
metaKeyValue = iso f g
  where
    f = \case Property x -> (x, Nothing); Setting x y -> (x, Just y)
    g = \case (x, Nothing) -> Property x; (x, Just y) -> Setting x y

metaKey :: Lens' MetaItem Text
metaKey = metaKeyValue % _1

metaValueMaybe :: Lens' MetaItem (Maybe Text)
metaValueMaybe = metaKeyValue % _2

metaValue :: AffineTraversal' MetaItem Text
metaValue = metaValueMaybe % _Just
