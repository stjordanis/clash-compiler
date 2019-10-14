{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TypeInType             #-}

module Clash.Tests.Scaffold where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Clash.Primitives.Scaffold
import           Clash.Prelude hiding (undefined)

makeScaffold "testFunction" "testPrimitive" []
  [ [ ClkOut "d1clk1"
    , ClkIn  "d1clk2"
    , In     "d1i1" 1
    , Out    "d1o1" 2
    ]
  , [ ClkOut "d2clk1"
    , ClkIn  "d2clk2"
    , In     "d2i1" 1
    , In     "d2i2" 1
    , Out    "d2o1" 2
  ] ]

-- | Type check our generated functions/datatypes
exists :: Bool
exists = True
 where
  _i1 = testFunctionI @System @System clockGen clockGen
  _i2 = TestFunctionI @System @System clockGen clockGen 0 0 0

  _o = TestFunctionO @System @System clockGen clockGen 0 0

  _ = if True then testFunction @System @System _i1 else _o
  _ = if True then testFunction @System @System _i2 else _o

  _ = if True then testFunction# @System @System clockGen clockGen 0 0 0 else _o


tests :: TestTree
tests = testGroup "Clash.Tests.Scaffold"
  [ testCase "Generated functions/datatypes exist" $ exists @?= True
  ]


