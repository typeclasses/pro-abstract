module ProAbstract.Tag.HasTag
    ( HasTag (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Tag.TagType

class HasTag x where
    type TagOpticKind x :: OpticKind
    tag :: Optic' (TagOpticKind x) NoIx x (Tag (Annotation x))

instance HasTag (Tag ann) where
    type TagOpticKind (Tag ann) = A_Lens
    tag = castOptic simple
