module ProAbstract.Tagless.KindOfText
    ( KindOfText (..)
    ) where

data KindOfText seq txt where
    TextLine       :: KindOfText seq           Text
    TextStanza     :: KindOfText seq      (seq Text)
    TextParagraphs :: KindOfText seq (seq (seq Text))
