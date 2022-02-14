module ProAbstract.Annotation.HasManyAnnotations
    ( HasManyAnnotations (..)
    ) where

import ProAbstract.Annotation.AnnotationFamily

class HasManyAnnotations x x' where
    allAnnotations :: Traversal x x' (Annotation x) (Annotation x')
