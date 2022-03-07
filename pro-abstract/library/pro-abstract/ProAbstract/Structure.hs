module ProAbstract.Structure
    (
    -- * Document
      Document (..)

    -- * Blocks
    , Block (..), Blocks (..), BlockTag (..)
    , TaggedBlocks (..), BlockTagContent (..)
    , TaggedPlainBlock (..)

    -- * Paragraph
    , Paragraph (..)

    -- * Inlines
    , Inline (..), Line (..), Lines (..), TaggedLines (..)

    -- * Text
    , PlainBlock (..), Fragment (..)

    -- * Plain or fork
    , CanBePlain (..), Plain, CanFork (..), Fork

    -- * Tagged or bare
    , IsTaggedOrBare (..), TaggedOrBare (..), tagged, bare

    -- * Traversals
    , HasManyParagraphs (..), HasManyPlainBlocks (..), HasManyPlainInlines (..)

    ) where


import ProAbstract.Structure.Block
import ProAbstract.Structure.BlockTag
import ProAbstract.Structure.BlockTagContent
import ProAbstract.Structure.CanBePlain
import ProAbstract.Structure.CanFork
import ProAbstract.Structure.Document
import ProAbstract.Structure.Fork
import ProAbstract.Structure.Fragment
import ProAbstract.Structure.HasManyParagraphs
import ProAbstract.Structure.HasManyPlainBlocks
import ProAbstract.Structure.HasManyPlainInlines
import ProAbstract.Structure.Inline
import ProAbstract.Structure.IsTaggedOrBare
import ProAbstract.Structure.Paragraph
import ProAbstract.Structure.Plain
import ProAbstract.Structure.PlainBlock
