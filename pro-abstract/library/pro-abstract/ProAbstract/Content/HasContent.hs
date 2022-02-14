module ProAbstract.Content.HasContent
    ( HasContent (..), HasContent'
    ) where

import ProAbstract.Content.ContentFamily

class HasContent x x' where
    content :: Lens x x' (Content x) (Content x')

type HasContent' x = HasContent x x
