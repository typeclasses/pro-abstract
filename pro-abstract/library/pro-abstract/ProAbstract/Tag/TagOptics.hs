module ProAbstract.Tag.TagOptics where

import ProAbstract.Tag.TagType

name :: Lens' (Tag ann) Text
name = lens tagName (\t n -> t { tagName = n })
