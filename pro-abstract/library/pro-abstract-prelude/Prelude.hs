module Prelude (module X) where

import Control.Applicative as X ((<|>))
import Control.DeepSeq as X (NFData)
import Data.Function as X (on)
import Data.Functor as X ((<&>))
import Data.Functor.Identity as X (Identity (Identity), runIdentity)
import Data.Hashable as X (Hashable)
import Data.Kind as X (Type)
import Data.Map as X (Map)
import Data.Maybe as X (fromMaybe, mapMaybe)
import Data.Sequence as X (Seq)
import Data.Set as X (Set)
import Data.Text as X (Text)
import GHC.Exts as X (IsList (..))
import GHC.Generics as X (Generic)

import Containers as X

import BasePrelude as X (Applicative, Bool (..), Either (..), Eq ((==)), Foldable, Functor, Maybe (..), Monad, Monoid (mempty), Ord (compare), Ordering (EQ), Semigroup (..), Show, Traversable, const, either, fmap, maybe, not, pure, traverse, ($), (&&), (.), (<$>), (<*>), (=<<), (>>=), (||))

import Optics.Core as X (A_Fold, A_Getter, A_Lens, A_Traversal, AffineFold, AffineTraversal, AffineTraversal', An_AffineFold, An_AffineTraversal, An_Iso, Getter, Is, Iso, Iso', JoinKinds, Lens, Lens', NoIx, Optic, Optic', OpticKind, Prism', Traversal, Traversal', _1, _2, _Just, _Left, _Right, adjoin, afailing, afolding, atraversal, castOptic, coerced, equality, iso, lens, matching, over, preview, prism', re, review, set, simple, to, traversalVL, traverseOf, traversed, view, (%))
import Optics.Empty.Core as X (_Empty, pattern Empty)
