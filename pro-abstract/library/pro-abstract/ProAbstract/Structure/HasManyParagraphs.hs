module ProAbstract.Structure.HasManyParagraphs
    ( HasManyParagraphs (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Structure.Paragraph

class HasManyParagraphs x where
    allParagraphs :: Traversal' x (Paragraph (Annotation x))
