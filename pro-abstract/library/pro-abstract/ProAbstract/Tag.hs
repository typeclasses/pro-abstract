module ProAbstract.Tag
    ( Tag (..), name
    , HasTag (..)
    , HasManyTags (..), HasWitherableTags (..)
    , HasManyBlockTags (..), HasWitherableBlockTags (..)
    , HasWitherableInlineTags (..)
    ) where

import ProAbstract.Tag.HasManyTags
import ProAbstract.Tag.HasTag
import ProAbstract.Tag.HasTagOptics
import ProAbstract.Tag.TagType
