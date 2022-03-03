module ProAbstract.Optics
    (
    -- * Content
      content, contents

    -- * Tags
    , tag, name
    , allTags, allInlineTags, allBlockTags

    -- * Annotation
    , annotation, allAnnotations

    -- * Metadata
    , metadata
    , atSetting, hasProperty, properties, settings
    , allMetadata, metaList, metaMap

    -- * Structure
    , fork, plain, tagged, bare, taggedOrBare
    , allParagraphs, allPlainBlocks, allPlainInlines

    -- * Taglessness
    , tagless, taglessContent, KindOfText (..)

    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Metadata
import ProAbstract.Structure
import ProAbstract.Tag
import ProAbstract.Tagless
