module ProAbstract.Structure.BlockTag
    ( BlockTag (..), blockTag
    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Metadata
import ProAbstract.Structure.Block
import ProAbstract.Structure.BlockTagContent
import ProAbstract.Structure.CanBePlain
import ProAbstract.Structure.CanFork
import ProAbstract.Structure.Fork
import ProAbstract.Structure.HasManyParagraphs
import ProAbstract.Structure.HasManyPlainBlocks
import ProAbstract.Structure.HasManyPlainInlines
import ProAbstract.Structure.Plain
import ProAbstract.Structure.PlainBlock
import ProAbstract.Tag

data BlockTag ann =
    BlockTagFork  (Tagged (Blocks ann))     -- ^ 'ProAbstract.fork'
  | BlockTagPlain (Tagged (PlainBlock ann)) -- ^ 'ProAbstract.plain'
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Content (BlockTag ann) = BlockTagContent ann

type instance Annotation (BlockTag ann) = ann

type instance Plain (BlockTag ann) = Tagged (PlainBlock ann)

type instance Fork (BlockTag ann) = Tagged (Blocks ann)

instance HasContent (BlockTag ann) (BlockTag ann) where
    content = lens f g
      where
        f = \case
            BlockTagFork x -> BlockTagContent_Fork $ view content x
            BlockTagPlain x -> BlockTagContent_Plain $ view content x
        g x = \case
            BlockTagContent_Fork c -> BlockTagFork $ TaggedBlocks t c
            BlockTagContent_Plain c -> BlockTagPlain $ TaggedPlainBlock t c
          where
            t = view tag x

instance CanFork (BlockTag ann) where
    fork = prism' BlockTagFork \case{ BlockTagFork x -> Just x; _ -> Nothing }

instance CanBePlain (BlockTag ann) where
    plain = prism' BlockTagPlain \case{ BlockTagPlain x -> Just x; _ -> Nothing }

instance HasAnnotation (BlockTag ann) (BlockTag ann) where
    annotation = lens f g
      where
        f = \case
            BlockTagFork x -> view annotation x
            BlockTagPlain x -> view annotation x
        g = \case
            BlockTagFork x -> \a -> BlockTagFork $ set annotation a x
            BlockTagPlain x -> \a -> BlockTagPlain $ set annotation a x

instance HasManyAnnotations (BlockTag ann) (BlockTag ann') where
    allAnnotations = traversalVL \f -> \case
        BlockTagFork x -> BlockTagFork <$> traverseOf allAnnotations f x
        BlockTagPlain x -> BlockTagPlain <$> traverseOf allAnnotations f x

instance HasManyMetadata (BlockTag ann) where
    allMetadata = (fork % allMetadata) `adjoin` (plain % metadata)

instance HasManyParagraphs (BlockTag ann) where
    allParagraphs = fork % allParagraphs

instance HasManyPlainBlocks (BlockTag ann) where
    allPlainBlocks = (fork % allPlainBlocks) `adjoin` plain

instance HasManyPlainInlines (BlockTag ann) where
    allPlainInlines = fork % allPlainInlines

instance HasManyTags (BlockTag ann) where
    allTags = (fork % allTags) `adjoin` (plain % tag)
    allInlineTags = fork % allInlineTags

instance HasTag (BlockTag ann) where
    type TagOpticKind (BlockTag ann) = A_Lens
    tag = lens f g
      where
        f = \case
            BlockTagFork x -> view tag x
            BlockTagPlain x -> view tag x
        g = \case
            BlockTagFork x -> \a -> BlockTagFork (set tag a x)
            BlockTagPlain x -> \a -> BlockTagPlain (set tag a x)

instance HasMetadata (BlockTag ann) where
    type MetadataOpticKind (BlockTag ann) = A_Lens
    metadata = tag % metadata

blockTag :: Prism' (Block ann) (BlockTag ann)
blockTag = prism' f g
  where
    f = \case
      BlockTagFork x -> BlockFork x
      BlockTagPlain x -> BlockPlain x
    g = \case
      BlockFork x -> Just (BlockTagFork x)
      BlockPlain x -> Just (BlockTagPlain x)
      _ -> Nothing
