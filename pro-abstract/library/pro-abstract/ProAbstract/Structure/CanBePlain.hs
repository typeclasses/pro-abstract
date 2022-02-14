module ProAbstract.Structure.CanBePlain
    ( CanBePlain (..)
    ) where

import ProAbstract.Structure.Plain

class CanBePlain x where
    plain :: Prism' x (Plain x)
