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
      if isEventPedal then
        Input.EventPedal <$> arbitrary <*> arbitrary
      else
        Input.EventExpr <$> arbitrary <*> choose (0, 127)

  shrink (Input.EventPedal pedal state) = concat [
        [ Input.EventPedal pedal' state  | pedal' <- shrink pedal ]
      , [ Input.EventPedal pedal  state' | state' <- shrink state ]
      ]
  shrink (Input.EventExpr expr value) = concat [
        [ Input.EventPedal Input.Pedal1 Input.Press ]
      , [ Input.EventExpr expr' value  | expr'  <- shrink expr  ]
      , [ Input.EventExpr expr  value' | value' <- shrink value ]
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
