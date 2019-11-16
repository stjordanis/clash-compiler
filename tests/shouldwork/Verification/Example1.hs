{-# LANGUAGE OverloadedStrings #-}

module Example1 where

import           Data.Coerce

import           Clash.Explicit.Prelude          hiding (assert)
import           Clash.Explicit.Testbench        hiding (assert)
import           Clash.Verification
import           Clash.Verification.DSL
import           Clash.Verification.Types
import           Clash.XException                (hwSeqX)

topEntity
  :: Clock System
  -> Signal System Bool
  -> Signal System Bool
  -> Signal System CvResult
topEntity clk a b =
  assert clk "MY_PROP" PSL $ always $ (a ~> (not <$> b))
{-# NOINLINE topEntity #-}

assertCvResult
  :: forall a dom
   . (Bounded n, Enum n, Eq n, NFDataX n, KnownDomain dom)
  => Clock dom -> Reset dom -> Enable dom
  -> n
  -> Signal dom CvResult
  -> Signal dom Bool
assertCvResult clk rst gen max results = done
 where
  counter = register clk rst gen (minBound :: a) counter
  done = (\(cvRes, c) -> cvRes `hwSeqX` c == max) <$> bundle (results, counter)
{-# NOINLINE assertCvResult #-}

testBench :: Signal System Bool
testBench = done
  where
    done = assertCvResult clk rst gen (maxBound :: Index 3) out
    out  = topEntity clk a b
    a    = stimuliGenerator clk rst (False :> True :> True :> Nil)
    b    = stimuliGenerator clk rst (False :> False :> True :> Nil)
    clk  = tbSystemClockGen (not <$> done)
    rst  = systemResetGen
    gen  = enableGen
