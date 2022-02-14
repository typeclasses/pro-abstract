module ProAbstract.Structure.Fragment
    ( Fragment (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Content

data Fragment ann = Fragment
  { fragmentText :: Text -- ^ 'ProAbstract.content'
  , fragmentAnnotation :: ann -- ^ 'ProAbstract.annotation'
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (Fragment ann) = ann

instance HasAnnotation (Fragment ann) (Fragment ann') where
    annotation = lens fragmentAnnotation \x a -> x { fragmentAnnotation = a }

type instance Content (Fragment ann) = Text

instance HasContent (Fragment ann) (Fragment ann) where
    content = lens fragmentText (\f t -> f { fragmentText = t })
