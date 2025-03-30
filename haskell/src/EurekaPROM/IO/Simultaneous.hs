-- | Support simultaneous pedal presses
--
-- Intended for qualified import.
--
-- > import EurekaPROM.IO.Simultaneous (simultaneous)
-- > import EurekaPROM.IO.Simultaneous qualified as Simultaneous
module EurekaPROM.IO.Simultaneous (
    MealyMachine
  , DeviceState(..)
  , initDeviceState
  , simultaneous
  ) where

import Data.Mealy (Mealy)
import Data.Mealy qualified as Mealy

import EurekaPROM.IO.Input

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

-- | Current state of the board
--
-- On the assumption that the user does not have more than two feet, we support
-- up to two pressed pedals.
data DeviceState =
    StateNoPedals

    -- One pedal pressed
  | StateOnePedal Pedal

    -- Two pedals pressed
    --
    -- We don't need to remember the order in which both pedals are pressed, so
    -- we maintain the invariant that @pedalPressed1 < pedalPressed2@.
  | StateTwoPedals Pedal Pedal
  deriving stock (Show, Eq, Ord)

initDeviceState :: DeviceState
initDeviceState = StateNoPedals

{-------------------------------------------------------------------------------
  Mealy machine

  A press or release of a pedal sends the expected signal when no other pedals
  are pressed at the same time.

  If another pedal @pedal1@ /is/ pressed, then

  1. if @pedal2 < pedal1@, a press or release of @pedal2@ results in no events
  2. if @pedal2 > pedal1@,
     (a) a press of @pedal2@ will result in the expected event
     (b) a release of @pedal2@ will result in a /press/ event for @pedal1@

  That means that 'Release' events always mean what they say, but 'Press' events
  do not.
-------------------------------------------------------------------------------}

type MealyMachine = Mealy DeviceState PedalEvent [PedalEvent]

simultaneous :: MealyMachine
simultaneous = Mealy.fromTransitions $ concat [
      --
      -- Transitions from 'StateNoPedal'
      --

      [ Mealy.Transition{
            from = ( StateNoPedals
                   , PedalEvent pedal Press
                   )
          , to   = ( StateOnePedal pedal
                   , [PedalEvent pedal Press]
                   )
          }
      | pedal <- allPedals
      ]

      --
      -- Transitions from 'StateOnePedal'
      --

    , [ Mealy.Transition{
            from  = ( StateOnePedal pedal
                    , PedalEvent pedal Release
                    )
          , to    = ( StateNoPedals
                    , [PedalEvent pedal Release]
                    )
          }
      | pedal <- allPedals
      ]
    , [ Mealy.Transition{
            from  = ( StateOnePedal pedal1
                    , PedalEvent pedal2 Press
                    )
          , to    = if pedal2 > pedal1 then
                      ( StateTwoPedals pedal1 pedal2
                      , [PedalEvent pedal2 Press]
                      )
                    else
                      ( StateOnePedal pedal2
                      , [ PedalEvent pedal2 Press
                        , PedalEvent pedal1 Release
                        ]
                      )
          }
      | pedal1 <- allPedals
      , pedal2 <- allPedals
      , pedal1 /= pedal2
      ]

      --
      -- Transitions from 'StateTwoPedals'
      --

    , [ Mealy.Transition{
            from = ( StateTwoPedals pedal1 pedal2
                   , PedalEvent pedal2 Release
                   )
          , to   = ( StateNoPedals
                   , [ PedalEvent pedal1 Release
                     , PedalEvent pedal2 Release
                     ]
                   )
          }
      | pedal1 <- allPedals
      , pedal2 <- allPedals
      , pedal1 < pedal2
      ]
    , [ Mealy.Transition{
            from  = ( StateTwoPedals pedal1 pedal2
                    , PedalEvent pedal3 Press
                    )
          , to    = ( StateOnePedal pedal3
                    , if pedal3 == pedal1 then
                        [PedalEvent pedal2 Release]
                      else
                        [ PedalEvent pedal1 Release
                        , PedalEvent pedal3 Press
                        , PedalEvent pedal2 Release
                        ]
                    )
          }
      | pedal1 <- allPedals
      , pedal2 <- allPedals
      , pedal1 < pedal2
      , pedal3 <- allPedals
      , pedal3 < pedal2
      ]
    , [ Mealy.Transition{
            from  = ( StateTwoPedals pedal1 pedal2
                    , PedalEvent pedal3 Press
                    )
          , to    = ( StateTwoPedals pedal2 pedal3
                    , [ PedalEvent pedal1 Release
                      , PedalEvent pedal3 Press
                      ]
                    )
          }
      | pedal1 <- allPedals
      , pedal2 <- allPedals
      , pedal1 < pedal2
      , pedal3 <- allPedals
      , pedal3 > pedal2
      ]
    ]

allPedals :: [Pedal]
allPedals = [minBound .. maxBound]
