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
        (plain % taglessContent @(Tagged (PlainBlock ann)) s) `afailing`
        (fork % taglessContent @(Tagged (Blocks ann)) s) `afailing`
        (paragraph % tagless @(Paragraph ann) s)

instance CanHaveTaglessContent (BlockTag ann) where
    taglessContent s =
        (plain % taglessContent @(Tagged (PlainBlock ann)) s) `afailing`
        (fork % taglessContent @(Tagged (Blocks ann)) s)

instance CanHaveTaglessContent (Inline ann) where
    taglessContent s = (plain % tagless s) `afailing` (fork % taglessContent s)

instance CanHaveTaglessContent (Lines ann) where
    taglessContent = tagless

instance CanHaveTaglessContent (Blocks ann) where
    taglessContent = tagless

instance CanHaveTaglessContent (Line ann) where
    taglessContent = tagless

instance CanHaveTaglessContent (Fragment ann) where
    taglessContent = tagless

instance CanHaveTaglessContent (Tagged (Blocks ann)) where
    taglessContent s = content % tagless s

instance CanHaveTaglessContent (Tagged (PlainBlock ann)) where
    taglessContent s = content % tagless s

instance CanHaveTaglessContent (Tagged (Lines ann)) where
    taglessContent s = content % tagless s

instance CanHaveTaglessContent (Document ann) where
    taglessContent s = content % tagless s
