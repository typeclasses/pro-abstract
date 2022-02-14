module ProAbstract.Structure.Block
    ( Block (..), paragraph, Blocks (..), Tagged (..)
    ) where

import ProAbstract.Annotation
import ProAbstract.Content
import ProAbstract.Structure.Fork
import ProAbstract.Metadata
import ProAbstract.Structure.CanBePlain
import ProAbstract.Structure.CanFork
import ProAbstract.Structure.HasManyParagraphs
import ProAbstract.Structure.HasManyPlainBlocks
import ProAbstract.Structure.HasManyPlainInlines
import ProAbstract.Structure.Paragraph
import ProAbstract.Structure.Plain
import ProAbstract.Structure.PlainBlock
import ProAbstract.Tag


-- ⭐ Block

data Block ann =
    BlockPlain     (Tagged (PlainBlock ann)) -- ^ 'ProAbstract.plain'
  | BlockParagraph (Paragraph ann)           -- ^ 'ProAbstract.paragraph'
  | BlockFork      (Tagged (Blocks ann))     -- ^ 'ProAbstract.fork'
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (Block ann) = ann

type instance Plain (Block ann) = Tagged (PlainBlock ann)

type instance Fork (Block ann) = Tagged (Blocks ann)

instance HasMetadata (Block ann) where
    type MetadataOpticKind (Block ann) = An_AffineTraversal
    metadata = tag % metadata

instance HasManyAnnotations (Block ann) (Block ann') where
    allAnnotations = traversalVL \f -> \case
        BlockPlain     x -> BlockPlain     <$> traverseOf allAnnotations f x
        BlockParagraph x -> BlockParagraph <$> traverseOf allAnnotations f x
        BlockFork      x -> BlockFork      <$> traverseOf allAnnotations f x

instance HasAnnotation (Block ann) (Block ann) where
    annotation = lens f g
      where
        f = \case
            BlockPlain     x -> view annotation x
            BlockParagraph x -> view annotation x
            BlockFork      x -> view annotation x
        g = \case
            BlockPlain     x -> \a -> BlockPlain     (set annotation a x)
            BlockParagraph x -> \a -> BlockParagraph (set annotation a x)
            BlockFork      x -> \a -> BlockFork      (set annotation a x)

instance HasManyPlainInlines (Block ann) where
    allPlainInlines = allParagraphs % allPlainInlines

instance HasManyParagraphs (Block ann) where
    allParagraphs = paragraph `adjoin` (fork % allParagraphs)

instance HasManyPlainBlocks (Block ann) where
    allPlainBlocks = plain `adjoin` (fork % allPlainBlocks)

instance HasTag (Block ann) where
    type TagOpticKind (Block ann) = An_AffineTraversal
    tag = atraversal f g
      where
        f = \case
            BlockPlain x -> Right (view tag x)
            BlockFork  x -> Right (view tag x)
            x -> Left x
        g = \case
            BlockPlain x -> \a -> BlockPlain (set tag a x)
            BlockFork  x -> \a -> BlockFork  (set tag a x)
            x -> \_ -> x

instance HasManyTags (Block ann) where
    allTags = (fork % allTags) `adjoin` (paragraph % allTags) `adjoin` (plain % tag)
    allInlineTags = allParagraphs % allInlineTags

instance HasManyBlockTags (Block ann) where
    allBlockTags = (fork % allBlockTags) `adjoin` (plain % tag)

instance HasWitherableInlineTags (Block ann) where
    witherInlineTags f = traverseOf allParagraphs (witherInlineTags f)

instance HasManyMetadata (Block ann) where
    allMetadata = allTags % metadata

instance CanFork (Block ann) where
    fork = prism'
        BlockFork
        \case{ BlockFork t -> Just t; _ -> Nothing }

instance CanBePlain (Block ann) where
    plain = prism'
        BlockPlain
        \case{ BlockPlain t -> Just t; _ -> Nothing }

paragraph :: Prism' (Block ann) (Paragraph ann)
paragraph = prism'
    BlockParagraph
    \case{ BlockParagraph p -> Just p; _ -> Nothing }


-- ⭐ Blocks

newtype Blocks ann =
    Blocks
      (Seq (Block ann)) -- ^ 'ProAbstract.content'
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)
  deriving newtype (Semigroup, Monoid)

type instance Contents (Blocks ann) = Block ann

instance HasContents (Blocks ann) (Blocks ann') where
    contents = castOptic coerced

instance IsList (Blocks ann) where
    type Item (Blocks ann) = Block ann
    toList (Blocks xs) = toList xs
    fromList xs = Blocks (fromList xs)

type instance Annotation (Blocks ann) = ann

instance HasManyAnnotations (Blocks ann) (Blocks ann') where
    allAnnotations = contents % traversed % allAnnotations

instance HasManyPlainInlines (Blocks ann) where
    allPlainInlines = contents % traversed % allPlainInlines

instance HasManyPlainBlocks (Blocks ann) where
    allPlainBlocks = contents % traversed % allPlainBlocks

instance HasManyTags (Blocks ann) where
    allTags = contents % traversed % allTags
    allInlineTags = contents % traversed % allInlineTags

instance HasManyBlockTags (Blocks ann) where
    allBlockTags = contents % traversed % allBlockTags

instance HasManyMetadata (Blocks ann) where
    allMetadata = allTags % metadata

instance HasManyParagraphs (Blocks ann) where
    allParagraphs = contents % traversed % allParagraphs

instance HasWitherableTags (Blocks ann) where
    witherTags f = traverseOf contents $ seqWither \case
        BlockParagraph x ->
            Just . BlockParagraph <$> traverseOf content (witherTags f) x
        BlockPlain x ->
            f (view tag x) >>= \case
                Nothing -> pure Nothing
                Just t -> pure . Just . BlockPlain . set tag t $ x
        BlockFork x ->
            f (view tag x) >>= \case
                Nothing -> pure Nothing
                Just t -> Just . BlockFork . set tag t <$> traverseOf content (witherTags f) x

instance HasWitherableBlockTags (Blocks ann) where
    witherBlockTags f = traverseOf contents $ seqWither \case
        BlockParagraph x -> pure $ Just $ BlockParagraph x
        BlockPlain x ->
            f (view tag x) >>= \case
                Nothing -> pure Nothing
                Just t -> pure . Just . BlockPlain . set tag t $ x
        BlockFork x ->
            f (view tag x) >>= \case
                Nothing -> pure Nothing
                Just t -> Just . BlockFork . set tag t <$> traverseOf content (witherBlockTags f) x

instance HasWitherableInlineTags (Blocks ann) where
    witherInlineTags f = traverseOf (contents % traversed) (witherInlineTags f)


-- ⭐ Tagged Blocks

data instance Tagged (Blocks ann) =
  TaggedBlocks
    { blocksTag :: Tag ann -- ^ 'ProAbstract.tag'
    , taggedBlocks :: Blocks ann -- ^ 'ProAbstract.content'
    }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, NFData)

type instance Annotation (Tagged (Blocks ann)) = ann

instance HasTag (Tagged (Blocks ann)) where
    type TagOpticKind (Tagged (Blocks ann)) = A_Lens
    tag = lens blocksTag \x a -> x{ blocksTag = a }

type instance Content (Tagged (Blocks ann)) = Blocks ann

type instance Contents (Tagged (Blocks ann)) = Block ann

instance HasManyAnnotations (Tagged (Blocks ann)) (Tagged (Blocks ann')) where
    allAnnotations = traversalVL \f (TaggedBlocks t b) -> TaggedBlocks
        <$> traverseOf annotation f t <*> traverseOf allAnnotations f b

instance HasAnnotation (Tagged (Blocks ann)) (Tagged (Blocks ann)) where
     annotation = tag % annotation

instance HasContent (Tagged (Blocks ann)) (Tagged (Blocks ann)) where
    content = lens taggedBlocks \x c -> x{ taggedBlocks = c }

instance HasContents (Tagged (Blocks ann)) (Tagged (Blocks ann)) where
    contents = content % contents

instance HasMetadata (Tagged (Blocks ann)) where
    type MetadataOpticKind (Tagged (Blocks ann)) = A_Lens
    metadata = tag % metadata

instance HasManyPlainInlines (Tagged (Blocks ann)) where
    allPlainInlines = content % allPlainInlines

instance HasManyTags (Tagged (Blocks ann)) where
    allTags = tag `adjoin` (content % allTags)
    allInlineTags = allParagraphs % allInlineTags

instance HasManyBlockTags (Tagged (Blocks ann)) where
    allBlockTags = tag `adjoin` (content % allBlockTags)

instance HasWitherableInlineTags (Tagged (Blocks ann)) where
    witherInlineTags f = traverseOf allParagraphs (witherInlineTags f)

instance HasManyMetadata (Tagged (Blocks ann)) where
    allMetadata = allTags % metadata

instance HasManyParagraphs (Tagged (Blocks ann)) where
    allParagraphs = content % allParagraphs

instance HasManyPlainBlocks (Tagged (Blocks ann)) where
    allPlainBlocks = content % allPlainBlocks
