module ProAbstract.Classes
    (
    -- * Block and inline choices
      CanBePlain (..), CanFork (..), IsTaggedOrBare (..)

    -- * Plaintext traversals
    , HasManyPlainBlocks (..), HasManyPlainInlines (..)

    -- * Annotation
    , HasAnnotation (..), HasAnnotation', HasManyAnnotations (..)

    -- * Content
    , HasContent (..), HasContent', HasContents (..), HasContents'

    -- * Metadata
    , HasMetadata (..), HasManyMetadata (..)

    -- * Tags
    , HasTag (..)

    -- ** Traversal
    , HasManyTags (..), HasManyBlockTags (..)

    -- ** Withering
    , HasWitherableTags (..), HasWitherableInlineTags (..), HasWitherableBlockTags (..)

    -- * Paragraphs
    , HasManyParagraphs (..)

    -- * Getting text from tagless content
    , CanBeTagless (..), CanHaveTaglessContent (..), KindOfText (..)

    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Metadata
import ProAbstract.Structure
import ProAbstract.Tag
import ProAbstract.Tagless
