module ProAbstract.Tagless.CanBeTagless
    ( CanBeTagless (..)
    ) where

import ProAbstract.Content
import ProAbstract.Tagless.KindOfText
import ProAbstract.Structure

class CanBeTagless a where
    tagless :: KindOfText txt -> AffineFold a txt

instance CanBeTagless (Fragment ann) where
    tagless = liftLine $ castOptic content

instance CanBeTagless (Line ann) where
    tagless = liftLine $ contents % all (tagless TextLine) % to textConcat

instance CanBeTagless (Lines ann) where
    tagless = linesAre $ tagless TextLine

instance CanBeTagless (PlainBlock ann) where
    tagless = linesAre $ tagless TextLine

instance CanBeTagless (Paragraph ann) where
    tagless = linesAre $ tagless TextLine

-- | Only a 'plain' inline is considered to be plain text, because 'fork' inlines have a tags.
instance CanBeTagless (Inline ann) where
    tagless s = plain % tagless s

-- | Only a 'bare' block is considered to be plain text, because 'fork' and 'plain' blocks have tags.
instance CanBeTagless (Block ann) where
    tagless s = bare % tagless s

instance CanBeTagless (Blocks ann) where
    tagless = \case
        TextParagraphs -> contents % all (tagless TextParagraphs) % to seqConcat
        s -> contents
            % seqAtMostOne -- For lines and stanza return types, there is not allowed to be more than one block
            % all @Maybe (tagless s) -- If there is a block, it has to be convertable to the desired text kind
            % to (fromMaybe (noText s)) -- If there are no blocks, return an empty line or stanza

instance CanBeTagless (BlockTagContent ann) where
    tagless s = (fork % tagless s) `afailing` (plain % tagless s)

noText :: KindOfText txt -> txt
noText = \case{ TextLine -> textEmpty; TextStanza -> Empty; TextParagraphs -> Empty }

all :: Traversable t => AffineFold a b -> AffineFold (t a) (t b)
all = afolding . traverse . preview

liftLine :: AffineFold a Text -> KindOfText txt -> AffineFold a txt
liftLine o = liftStanza o (o % re seqSingleton)

liftStanza :: AffineFold a Text -> AffineFold a (Seq Text) -> KindOfText txt -> AffineFold a txt
liftStanza o1 o2 = \case{ TextLine -> o1; TextStanza -> o2; TextParagraphs -> o2 % re seqSingleton }

linesAre :: HasContents' a => AffineFold (Contents a) Text -> KindOfText txt -> AffineFold a txt
linesAre o = liftStanza (contents % seqAtMostOne % all o % to (fromMaybe textEmpty)) (contents % all o)
