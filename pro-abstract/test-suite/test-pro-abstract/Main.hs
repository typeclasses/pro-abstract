module Main (main) where

import ProAbstract

import Optics.Core
import Prelude hiding (break)

import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Traversable (for)
import Hedgehog (Gen, Property, checkParallel, discover, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import System.Exit (exitFailure)
import Hedgehog.Optics

main :: IO ()
main = checkParallel $$discover >>= \ok -> when (not ok) exitFailure

frag :: Text -> Fragment ()
frag x = Fragment{ fragmentText = x, fragmentAnnotation = () }

inlinePlain :: Text -> Inline ()
inlinePlain = InlinePlain . frag

inlineFork :: Lines () -> Inline ()
inlineFork x = InlineFork $ TaggedLines{ linesTag = Tag{ tagName = "x", tagMetadata = mempty, tagAnnotation = () }, taggedLines = x }

para :: Lines () -> Block ()
para x = BlockParagraph Paragraph{ paragraphAnnotation = (), paragraphContent = x }

btag :: Blocks () -> Block ()
btag x = BlockFork TaggedBlocks{ blocksTag = Tag{ tagName = "x", tagMetadata = mempty, tagAnnotation = () }, taggedBlocks = x }

tbtag :: Blocks () -> BlockTag ()
tbtag x = BlockTagFork TaggedBlocks{ blocksTag = Tag{ tagName = "x", tagMetadata = mempty, tagAnnotation = () }, taggedBlocks = x }

prop_ex1 :: Property
prop_ex1 = withTests 1 $ property $ do
    x <- pure $ inlinePlain "abc"
    preview (tagless TextStanza) x === Just ["abc"]

prop_ex2 :: Property
prop_ex2 = withTests 1 $ property $ do
    x <- pure $ InlineFork $
        TaggedLines
            { linesTag = Tag{ tagName = "x", tagMetadata = mempty, tagAnnotation = () }
            , taggedLines = [[inlinePlain "abc", inlinePlain "def"]]
            }
    preview (fork % content % (tagless TextStanza)) x === Just ["abcdef"]

prop_ex3 :: Property
prop_ex3 = withTests 1 $ property $ do
    x <- pure $ inlineFork [[inlinePlain "abc", inlinePlain "def"], [inlinePlain "ghi"]]
    preview (taglessContent TextStanza) x === Just ["abcdef", "ghi"]

prop_ex4 :: Property
prop_ex4 = withTests 1 $ property $ do
    x <- pure $ inlineFork [[inlinePlain "abc"]]
    preview (taglessContent TextStanza) x === Just ["abc"]

prop_ex5 :: Property
prop_ex5 = withTests 1 $ property $ do
    x <- pure $ inlineFork [[inlinePlain "abc", inlineFork []]]
    preview (taglessContent TextStanza) x === Nothing

prop_ex6 :: Property
prop_ex6 = withTests 1 $ property $ do
    x <- pure $ para [[inlinePlain "abc", inlineFork []]]
    preview (tagless TextStanza) x === Nothing

prop_ex7 :: Property
prop_ex7 = withTests 1 $ property $ do
    x <- pure $ para [[inlinePlain "abc", inlinePlain "def"]]
    preview (tagless TextStanza) x === Just ["abcdef"]

prop_ex8 :: Property
prop_ex8 = withTests 1 $ property $ do
    x <- pure $ btag [para [[inlinePlain "abc", inlinePlain "def"]]]
    preview (taglessContent TextStanza) x === Just ["abcdef"]

prop_ex9 :: Property
prop_ex9 = withTests 1 $ property $ do
    x <- pure $ ([para [[inlinePlain "abc", inlinePlain "def"]]] :: Blocks ())
    preview (tagless TextStanza) x === Just ["abcdef"]

prop_ex10 :: Property
prop_ex10 = withTests 1 $ property $ do
    x <- pure $ tbtag $ [para [[inlinePlain "abc"]]]
    preview (taglessContent TextStanza) x === Just ["abc"]

prop_ex11 :: Property
prop_ex11 = withTests 1 $ property $ do
    x <- pure $ tbtag $ [para [[inlinePlain "abc", inlinePlain "def"]]]
    preview (fork % content % (tagless TextStanza)) x === Just ["abcdef"]

prop_ex12 :: Property
prop_ex12 = withTests 1 $ property $ do
    x <- pure $ tbtag $ [para [[inlinePlain "abc", inlinePlain "def"], [inlinePlain "ghi"]]]
    preview (fork % content % (tagless TextStanza)) x === Just ["abcdef", "ghi"]

genMetadata :: Gen Metadata
genMetadata = Metadata <$> genProperties <*> genSettings

genProperties :: Gen (Set Text)
genProperties = Set.fromList <$> genMetaKeys

metaKeyChoices :: [Text]
metaKeyChoices = ["one", "two", "three", "four", "five", "six", "seven"]

metaValueChoices :: [Text]
metaValueChoices = ["alpha", "beta", "delta", "gamma", "phi"]

genMetaKeys :: Gen [Text]
genMetaKeys = Gen.subsequence metaKeyChoices

genSettingValue :: Gen Text
genSettingValue = Gen.element metaValueChoices

genMetaKey :: Gen Text
genMetaKey = Gen.element metaKeyChoices

genSettings :: Gen (Map Text Text)
genSettings = do
    ks <- genMetaKeys
    kvs <- for ks \k -> genSettingValue >>= \v -> pure (k, v)
    pure $ Map.fromList kvs

genMetaMap :: Gen (Map Text MetaValue)
genMetaMap = do
    ks <- genMetaKeys
    kvs <- for ks \k -> genMetaValue >>= \v -> pure (k, v)
    pure $ Map.fromList kvs

genMetaValue :: Gen MetaValue
genMetaValue = Gen.choice
    [ pure MetaValue_Property
    , pure MetaValue_Setting <*> genSettingValue
    , pure MetaValue_PropertyAndSetting <*> genSettingValue
    ]

prop_metaMap :: Property
prop_metaMap = property $ wellFormedIso genMetadata genMetaMap metaMap
