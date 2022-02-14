module ProAbstract.Datatypes
    (
    -- * Document
      Document (..)

    -- * Blocks
    , Block (..), Blocks (..)

    -- * Lines
    , Inline (..), Line (..), Lines (..)

    -- * Tags and tagged things
    , Tag (..), Tagged (..), BlockTag (..)

    -- * Paragraph
    , Paragraph (..)

    -- * Plain text
    , Fragment (..), PlainBlock (..)

    -- * Metadata
    , Metadata (..)

    ) where

import ProAbstract.Metadata
import ProAbstract.Structure
import ProAbstract.Tag
