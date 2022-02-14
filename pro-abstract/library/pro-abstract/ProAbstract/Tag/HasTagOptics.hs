module ProAbstract.Tag.HasTagOptics
    ( name
    ) where

import ProAbstract.Tag.HasTag

import qualified ProAbstract.Tag.TagOptics as Tag

name :: (HasTag x, JoinKinds (TagOpticKind x) A_Lens k) => Optic' k NoIx x Text
name = tag % Tag.name
