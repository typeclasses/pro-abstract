module ProAbstract.Content.HasContents
    ( HasContents (..), HasContents'
    ) where

import ProAbstract.Content.ContentsFamily
import ProAbstract.Sequence.SequenceFamily

class HasContents x x' where
    contents :: Lens x x' (Sequence x (Contents x)) (Sequence x (Contents x'))

type HasContents' x = HasContents x x
