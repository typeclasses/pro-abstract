module ProAbstract.Structure.HasManyPlainBlocks
    ( HasManyPlainBlocks (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Structure.PlainBlock

class HasManyPlainBlocks x where
    allPlainBlocks :: Traversal' x (Tagged (PlainBlock (Annotation x)))
