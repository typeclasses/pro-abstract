module ProAbstract.Structure.HasManyPlainInlines
    ( HasManyPlainInlines (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Structure.Fragment

class HasManyPlainInlines x where
    allPlainInlines :: Traversal' x (Fragment (Annotation x))
