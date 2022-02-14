module ProAbstract.Structure.CanFork
    ( CanFork (..)
    ) where

import ProAbstract.Structure.Fork

class CanFork x where
    fork :: Prism' x (Fork x)
