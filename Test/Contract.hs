{-# LANGUAGE Rank2Types #-}
module Test.Contract where

import Control.Applicative
import Control.Lens hiding (elements)
import Control.Lens.Review
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Identity (Identity, runIdentity)
import Data.Monoid -- (First, getFirst)
import Data.Profunctor (Choice, dimap, right')

import Test.QuickCheck (Gen, Property, forAll, quickCheck)
import qualified Test.QuickCheck as QC

-- ideas from http://proglang.informatik.uni-freiburg.de/jscontest/jscontest/

type Predicate a = a -> Bool

allPreds :: [Predicate a] -> Predicate a
allPreds preds x = all ($ x) preds

anyPred :: [Predicate a] -> Predicate a
anyPred preds x = any ($ x) preds

data Contract a = Contract {
  check :: Predicate a,
  gen   :: Gen a
  }

singleton :: Eq a => a -> Contract a
singleton x = Contract (== x) (return x)

elements :: Eq a => [a] -> Contract a
elements xs = Contract (`elem` xs) (QC.elements xs)

oneof :: [Contract a] -> Contract a
oneof cs = Contract (anyPred (fmap check cs)) (QC.oneof (fmap gen cs))

listOf :: Contract a -> Contract [a]
listOf c = Contract (all (check c)) (QC.listOf (gen c))

vectorOf :: Int -> Contract a -> Contract [a]
vectorOf k c = Contract (allPreds [((== k) . length), (all (check c))]) (QC.vectorOf k (gen c))


-- problematic for the checking part
-- worth checking the JSConTest version of this
--fnOf :: Contract a -> Contract b -> Contract (a -> b)
--fnOf ca cb = Contract


contractProp :: Show a => Contract a -> Property
contractProp c = forAll (gen c) (check c)


newtype ConstR a b = ConstR { runConstR :: (Const b a)}

instance Bifunctor ConstR where
  bimap f g = ConstR . bimap g f . runConstR

instance Profunctor ConstR where
  lmap f = ConstR . Const . getConst . runConstR
  rmap f = bimap id f

-- clone of preview from Control.Lens
getFrom :: ((a -> (Const (First a) a)) -> s -> (Const (First a) s)) -> s -> Maybe a
getFrom l s = getFirst $ getConst $ l (Const . First . Just) s

-- clone of review from Control.Lens
rev :: (ConstR a (Identity a) -> ConstR s (Identity s)) -> a -> s
rev l a = runIdentity $ getConst $ runConstR $ l (ConstR $ Const (Identity a) a)

-- creates another contract through a prism.  the contract only
-- matches if the prism matches.
through :: (forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)) -> Contract a -> Contract s
through l c = Contract
              ((== Just True) . fmap (check c) . getFrom l)
              (fmap (review l) (gen c))


--_json :: forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)
_json :: (FromJSON a, ToJSON a) => forall p f. (Choice p, Applicative f) => p a (f a) -> p ByteString (f ByteString)
_json = dimap to fro . right' where
  to  x = maybe (Left x) Right $ JSON.decode x
  fro = either pure (fmap JSON.encode)
--_json = prism' JSON.encode JSON.decode

-- JSON stuff
json :: (FromJSON a, ToJSON a) => Contract a -> Contract ByteString
json = through $ _json

-- XML stuff

