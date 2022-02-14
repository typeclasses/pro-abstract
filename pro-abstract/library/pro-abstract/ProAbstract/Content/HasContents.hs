module ProAbstract.Content.HasContents
    ( HasContents (..), HasContents'
    ) where

import ProAbstract.Content.ContentsFamily

class HasContents x x' where
    contents :: Lens x x' (Seq (Contents x)) (Seq (Contents x'))

type HasContents' x = HasContents x x
