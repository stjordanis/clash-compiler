module Main where

import Test.Tasty

import qualified Clash.Tests.Core.FreeVars
import qualified Clash.Tests.Scaffold

tests :: TestTree
tests = testGroup "Unittests"
  [ Clash.Tests.Core.FreeVars.tests
  , Clash.Tests.Scaffold.tests
  ]

main :: IO ()
main = defaultMain tests
