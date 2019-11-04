{-# LANGUAGE OverloadedStrings #-}

module Example1 where

import           Data.Coerce

import           Clash.Prelude
import           Clash.Explicit.Testbench        hiding (assert)
import           Clash.Verification
import           Clash.Verification.DSL
import           Clash.Verification.Types

topEntity
  :: Clock System
  -> Signal System Bool
  -> Signal System Bool
  -> Signal System CvResult
topEntity clk a b =
  assert clk "MY_PROP" PSL $ always $ (a `implies` (not <$> b))

testBench :: Signal System Bool
testBench = done
  where
    a              = stimuliGenerator clk rst (False :> True :> True :> Nil)
    b              = stimuliGenerator clk rst (False :> False :> True :> Nil)
    expectedOutput = outputVerifier' clk rst (repeat @4 True)
    done           = expectedOutput (cvResultToBool <$> topEntity clk a b)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
