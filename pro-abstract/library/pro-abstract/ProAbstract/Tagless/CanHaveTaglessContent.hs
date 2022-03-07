module ProAbstract.Tagless.CanHaveTaglessContent
    ( CanHaveTaglessContent (..)
    ) where

import ProAbstract.Content
import ProAbstract.Structure
import ProAbstract.Tagless.CanBeTagless
import ProAbstract.Tagless.KindOfText

class CanHaveTaglessContent a where
    taglessContent :: KindOfText t -> AffineFold a t

instance CanHaveTaglessContent (Block ann) where
    taglessContent s =
        (tagged % taglessContent @(BlockTag ann) s) `afailing`
        (bare % tagless @(Paragraph ann) s)

instance CanHaveTaglessContent (BlockTag ann) where
    taglessContent s =
        (plain % taglessContent @(TaggedPlainBlock ann) s) `afailing`
        (fork % taglessContent @(TaggedBlocks ann) s)

instance CanHaveTaglessContent (BlockTagContent ann) where
    taglessContent = tagless

instance CanHaveTaglessContent (Inline ann) where
    taglessContent s =
        (plain % tagless s) `afailing`
        (fork % taglessContent s)

instance CanHaveTaglessContent (Lines ann) where
    taglessContent = tagless

instance CanHaveTaglessContent (Blocks ann) where
    taglessContent = tagless

instance CanHaveTaglessContent (Line ann) where
    taglessContent = tagless

instance CanHaveTaglessContent (Fragment ann) where
    taglessContent = tagless

instance CanHaveTaglessContent (TaggedBlocks ann) where
    taglessContent s = content % tagless s

instance CanHaveTaglessContent (TaggedPlainBlock ann) where
    taglessContent s = content % tagless s

instance CanHaveTaglessContent (TaggedLines ann) where
    taglessContent s = content % tagless s

instance CanHaveTaglessContent (Document ann) where
    taglessContent s = content % tagless s
