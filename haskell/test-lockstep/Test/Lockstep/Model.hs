-- | Model
--
-- Intended for unqualified import.
module Test.Lockstep.Model (
    -- * Model
    Model(..)
  , initModel
  , visible
    -- * Actions
  , canPerform
  , possibleActions
  , execute
  ) where

import Data.Set (Set)
import Data.Set qualified as Set

import EurekaPROM.IO.Input

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Model = Model {
      -- | Currently pressed pedals
      pressed :: Set Pedal

      -- | Delayed 'Release' event, if any
    , delayed :: [PedalEvent]
    }
  deriving stock (Show)

initModel :: Model
initModel = Model {
      pressed = Set.empty
    , delayed = []
    }

visible :: Model -> Pedal -> Bool
visible Model{pressed} pedal = all (<= pedal) (Set.elems pressed)

cancels :: PedalEvent -> PedalEvent -> Bool
cancels (PedalEvent pedal1 state1) (PedalEvent pedal2 state2) = and [
      pedal1 == pedal2
    , or [ state1 == Press   && state2 == Release
         , state1 == Release && state2 == Press
         ]
    ]

delay :: PedalEvent -> [PedalEvent] -> [PedalEvent]
delay n []     = [n]
delay n (o:os) = if n `cancels` o
                   then os
                   else o : delay n os

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

canPerform :: Model -> PedalEvent -> Bool
canPerform Model{pressed} (PedalEvent pedal Press) =
    Set.notMember pedal pressed && Set.size pressed <= 1
canPerform Model{pressed} (PedalEvent pedal Release) =
    Set.member pedal pressed

possibleActions :: Model -> [PedalEvent]
possibleActions pressed = filter (canPerform pressed) . concat $ [
      [ PedalEvent pedal Press
      , PedalEvent pedal Release
      ]

      -- Testing with fewer pedals for more "collisions"
    | pedal <- [Pedal1 .. Pedal5] -- [minBound .. maxBound]
    ]

execute :: PedalEvent -> Model -> ([PedalEvent], Model)
execute event model@Model{pressed, delayed} = (
      if visible model (pedalEventPedal event)
        then delayed ++ [event]
        else []
    , Model{ pressed = case event of
                         PedalEvent pedal Press   -> Set.insert pedal pressed
                         PedalEvent pedal Release -> Set.delete pedal pressed
           , delayed = if visible model (pedalEventPedal event)
                         then []
                         else delay event delayed
           }
    )
