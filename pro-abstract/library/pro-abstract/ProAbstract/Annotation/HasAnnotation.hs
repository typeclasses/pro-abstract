module ProAbstract.Annotation.HasAnnotation
    ( HasAnnotation (..)
    , HasAnnotation'
    ) where

import ProAbstract.Annotation.AnnotationFamily

class HasAnnotation x x' where
    annotation :: Lens x x' (Annotation x) (Annotation x')

type HasAnnotation' x = HasAnnotation x x
