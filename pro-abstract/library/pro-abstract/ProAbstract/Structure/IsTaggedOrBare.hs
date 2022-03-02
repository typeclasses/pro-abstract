module ProAbstract.Structure.IsTaggedOrBare
    ( TaggedOrBare (..)
    , IsTaggedOrBare (..)
    , tagged, bare
    ) where

import ProAbstract.Structure.Block
import ProAbstract.Structure.BlockTag
import ProAbstract.Structure.Fragment
import ProAbstract.Structure.Inline
import ProAbstract.Structure.Paragraph

data TaggedOrBare a =
    IsTagged (TaggedType a)
  | IsBare (BareType a)

tagged' :: Prism' (TaggedOrBare a) (TaggedType a)
tagged' = prism' IsTagged \case{ IsTagged x -> Just x; _ -> Nothing }

bare' :: Prism' (TaggedOrBare a) (BareType a)
bare' = prism' IsBare \case{ IsBare x -> Just x; _ -> Nothing }

class IsTaggedOrBare a where

    type TaggedType a
    type BareType a

    taggedOrBare :: Iso' a (TaggedOrBare a)

tagged :: IsTaggedOrBare a => Prism' a (TaggedType a)
tagged = taggedOrBare % tagged'

bare :: IsTaggedOrBare a => Prism' a (BareType a)
bare = taggedOrBare % bare'

instance IsTaggedOrBare (TaggedOrBare a) where

    type TaggedType (TaggedOrBare a) = TaggedType a
    type BareType (TaggedOrBare a) = BareType a

    taggedOrBare = iso f g
      where
        f = \case
            IsTagged x -> IsTagged x
            IsBare x -> IsBare x
        g = \case
            IsTagged x -> IsTagged x
            IsBare x -> IsBare x

instance IsTaggedOrBare (Inline ann) where

    type TaggedType (Inline ann) = Tagged (Lines ann)
    type BareType (Inline ann) = Fragment ann

    taggedOrBare = iso f g
      where
        f = \case
            InlineFork x -> IsTagged x
            InlinePlain x -> IsBare x
        g = \case
            IsTagged x -> InlineFork x
            IsBare x -> InlinePlain x

instance IsTaggedOrBare (Block ann) where

    type TaggedType (Block ann) = BlockTag ann
    type BareType (Block ann) = Paragraph ann

    taggedOrBare = iso f g
      where
        f = \case
            BlockPlain x -> IsTagged (BlockTagPlain x)
            BlockFork x -> IsTagged (BlockTagFork x)
            BlockParagraph x -> IsBare x
        g = \case
            IsTagged (BlockTagPlain x) -> BlockPlain x
            IsTagged (BlockTagFork x) -> BlockFork x
            IsBare x -> BlockParagraph x
