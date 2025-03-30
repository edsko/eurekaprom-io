module Test.EurekaPROM.IO.Input (tests) where

import EurekaPROM.IO.Input

import Test.Tasty
import Test.Tasty.QuickCheck

import Test.EurekaPROM.IO.Arbitrary ()

{-------------------------------------------------------------------------------
  List of tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.EurekaPROM.IO.Input" [
      testProperty "fromMIDI_toMIDI" prop_fromMIDI_toMIDI
    ]

{-------------------------------------------------------------------------------
  Individual properties
-------------------------------------------------------------------------------}

prop_fromMIDI_toMIDI :: Event -> Property
prop_fromMIDI_toMIDI event =
        eventFromMIDI (eventToMIDI event)
    === Just event