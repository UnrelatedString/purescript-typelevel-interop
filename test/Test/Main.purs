module Test.Main where

import Prelude

import Effect (Effect)
import Test.QuickCheck ((===))
import Test.Spec (it)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.June.Pretty (prettyReporter)
import Test.Spec.QuickCheck (quickCheck)

import Type.Interop (class NatInt)

import Data.Reflectable (reflectType, reifyType)
import Data.Typelevel.Num.Sets (class Nat, toInt', reifyInt)
import Type.Proxy (Proxy(..))

reifiedNatInt :: Int -> Int
reifiedNatInt n = reifyInt n r
  where
  r :: forall n n'. NatInt n n' => n -> Int
  r _ = reflectType (Proxy @n')

-- reifiedIntNat :: Int -> Int
-- reifiedIntNat n = reifyType n (\(_ :: Proxy n) -> toInt' (Proxy @n))

main :: Effect Unit
main = runSpecAndExitProcess [prettyReporter] do
  pure unit
