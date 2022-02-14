module ProAbstract.Tagless.KindOfText
    ( KindOfText (..)
    ) where

data KindOfText txt where
    TextLine       :: KindOfText           Text
    TextStanza     :: KindOfText      (Seq Text)
    TextParagraphs :: KindOfText (Seq (Seq Text))

