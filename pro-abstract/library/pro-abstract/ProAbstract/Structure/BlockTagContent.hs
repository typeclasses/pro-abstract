module ProAbstract.Structure.BlockTagContent
    ( BlockTagContent (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Metadata
import ProAbstract.Structure.Block
import ProAbstract.Structure.CanBePlain
import ProAbstract.Structure.CanFork
import ProAbstract.Structure.Fork
import ProAbstract.Structure.HasManyParagraphs
import ProAbstract.Structure.HasManyPlainBlocks
import ProAbstract.Structure.HasManyPlainInlines
import ProAbstract.Structure.Plain
import ProAbstract.Structure.PlainBlock
import ProAbstract.Tag

data BlockTagContent ann =
    BlockTagContent_Fork  (Blocks ann)     -- ^ 'ProAbstract.fork'
  | BlockTagContent_Plain (PlainBlock ann) -- ^ 'ProAbstract.plain'

type instance Annotation (BlockTagContent ann) = ann

type instance Plain (BlockTagContent ann) = PlainBlock ann

type instance Fork (BlockTagContent ann) = Blocks ann

instance CanBePlain (BlockTagContent ann) where
    plain = prism' BlockTagContent_Plain \case
        BlockTagContent_Plain x -> Just x; _ -> Nothing

instance CanFork (BlockTagContent ann) where
    fork = prism' BlockTagContent_Fork \case
        BlockTagContent_Fork x -> Just x; _ -> Nothing

instance HasManyPlainBlocks (BlockTagContent ann) where
    allPlainBlocks = fork % allPlainBlocks

instance HasManyPlainInlines (BlockTagContent ann) where
    allPlainInlines = fork % allPlainInlines

instance HasManyAnnotations (BlockTagContent ann) (BlockTagContent ann') where
    allAnnotations = traversalVL \f -> \case
        BlockTagContent_Fork x -> BlockTagContent_Fork <$> traverseOf allAnnotations f x
        BlockTagContent_Plain x -> BlockTagContent_Plain <$> traverseOf allAnnotations f x

instance HasManyMetadata (BlockTagContent ann) where
    allMetadata = fork % allMetadata

instance HasManyTags (BlockTagContent ann) where
    allTags = fork % allTags
    allInlineTags = fork % allInlineTags

instance HasManyBlockTags (BlockTagContent ann) where
    allBlockTags = fork % allBlockTags

instance HasWitherableTags (BlockTagContent ann) where
    witherTags f = traverseOf fork (witherTags f)
    mapMaybeTags f = over fork (mapMaybeTags f)

instance HasWitherableInlineTags (BlockTagContent ann) where
    witherInlineTags f = traverseOf fork (witherInlineTags f)
    mapMaybeInlineTags f = over fork (mapMaybeInlineTags f)

instance HasWitherableBlockTags (BlockTagContent ann) where
    witherBlockTags f = traverseOf fork (witherBlockTags f)
    mapMaybeBlockTags f = over fork (mapMaybeBlockTags f)

instance HasManyParagraphs (BlockTagContent ann) where
    allParagraphs = fork % allParagraphs
