module ProAbstract.Datatypes
    (
    -- * Document
      Document (..)

    -- * Blocks
    , Block (..), Blocks (..), BlockTagContent (..)

    -- * Lines
    , Inline (..), Line (..), Lines (..)

    -- * Tags and tagged things
    , Tag (..), BlockTag (..), TaggedBlocks (..)
    , TaggedPlainBlock (..), TaggedLines (..), TaggedOrBare (..)

    -- * Paragraph
    , Paragraph (..)

    -- * Plain text
    , Fragment (..), PlainBlock (..)

    -- * Metadata
    , Metadata (..), MetaItem (..), MetaValue (..)

    ) where

import ProAbstract.Metadata
import ProAbstract.Structure
import ProAbstract.Tag
