{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'Arbitrary' (orphan) instances
module Test.EurekaPROM.IO.Arbitrary () where

import Test.QuickCheck

import EurekaPROM.IO.Input qualified as Input

{-------------------------------------------------------------------------------
  Input
-------------------------------------------------------------------------------}

instance Arbitrary Input.Event where
  arbitrary = do
      isEventPedal <- arbitrary
      if isEventPedal
        then Input.EventPedal <$> arbitrary
        else Input.EventExpr  <$> arbitrary

  shrink (Input.EventPedal event) = Input.EventPedal <$> shrink event
  shrink (Input.EventExpr  event) = concat [
        [ Input.EventPedal $ Input.PedalEvent Input.Pedal1 Input.Press ]
      , Input.EventExpr <$> shrink event
      ]

instance Arbitrary Input.PedalEvent where
  arbitrary = Input.PedalEvent <$> arbitrary <*> arbitrary
  shrink (Input.PedalEvent pedal state) = concat [
        [ Input.PedalEvent pedal' state  | pedal' <- shrink pedal ]
      , [ Input.PedalEvent pedal  state' | state' <- shrink state ]
      ]

instance Arbitrary Input.ExprEvent where
  arbitrary = Input.ExprEvent <$> arbitrary <*> choose (0, 127)
  shrink (Input.ExprEvent expr value) = concat [
        [ Input.ExprEvent expr' value  | expr'  <- shrink expr  ]
      , [ Input.ExprEvent expr  value' | value' <- shrink value ]
      ]

instance Arbitrary Input.Pedal where
  arbitrary = arbitraryEnum
  shrink    = shrinkEnum

instance Arbitrary Input.PedalState where
  arbitrary = arbitraryEnum
  shrink    = shrinkEnum

instance Arbitrary Input.Expr where
  arbitrary = arbitraryEnum
  shrink    = shrinkEnum

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

arbitraryEnum :: (Enum a, Bounded a) => Gen a
arbitraryEnum = elements [minBound .. maxBound]

shrinkEnum :: (Ord a, Enum a, Bounded a) => a -> [a]
shrinkEnum x = filter (< x) [minBound .. maxBound]
