module ProAbstract.Optics
    (
    -- * Lenses
      content, contents, tag, name

    -- ** Annotation
    , annotation

    -- ** Metadata
    , metadata, atSetting, hasProperty, properties, settings

    -- * Prisms
    , fork, plain, tagged, bare

    -- * Isomorphisms
    , taggedOrBare

    -- * Traversals
    , allParagraphs, allPlainBlocks, allPlainInlines

    -- ** Annotation
    , allAnnotations

    -- ** Tags
    , allTags, allInlineTags, allBlockTags

    -- ** Metadata
    , allMetadata

    -- * Affine folds
    , tagless, taglessContent, KindOfText (..)

    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Metadata
import ProAbstract.Structure
import ProAbstract.Tag
import ProAbstract.Tagless
