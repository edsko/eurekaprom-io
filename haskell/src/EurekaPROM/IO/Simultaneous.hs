-- | Support simultaneous pedal presses
--
-- Intended for qualified import.
--
-- > import EurekaPROM.IO.Simultaneous (simultaneous)
-- > import EurekaPROM.IO.Simultaneous qualified as Simultaneous
module EurekaPROM.IO.Simultaneous (
    DeviceState(..)
  , OnePedalPressed(..)
  , TwoPedalsPressed(..)
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
  | StateOnePedal OnePedalPressed
  | StateTwoPedals TwoPedalsPressed
  deriving stock (Show, Eq, Ord)

-- | One pedal pressed
data OnePedalPressed = OnePedalPressed {
      pedalPressed :: Pedal

      -- | Did we issue a 'Press' event for the pedal?
      --
      -- See Case 4, below.
    , pedalNotified :: Bool
    }
  deriving stock (Show, Eq, Ord)

-- | Two pedals pressed
--
-- We don't need to remember the order in which both pedals are pressed, so we
-- maintain the invariant that @pedalPressed1 < pedalPressed2@.
data TwoPedalsPressed = TwoPedalsPressed {
      pedalPressed1 :: Pedal
    , pedalPressed2 :: Pedal

      -- | Did we issue a 'Press' event for the pedal?
      --
      -- We can only get here from a 'OnePedalPressed' state where
      -- 'pedalNotified' is 'False'.
    , pedalNotified1 :: Bool
    }
  deriving stock (Show, Eq, Ord)

initDeviceState :: DeviceState
initDeviceState = StateNoPedals

{-------------------------------------------------------------------------------
  Mealy machine
-------------------------------------------------------------------------------}

simultaneous :: Mealy DeviceState Event [Event]
simultaneous = Mealy.fromTransitions $ concat [
      --
      -- The simple cases: one pedal only
      --
      concat [
        [ Mealy.Transition {
              from   = StateNoPedals
            , to     = StateOnePedal OnePedalPressed{
                           pedalPressed  = pedal
                         , pedalNotified = True
                         }
            , input  = EventPedal pedal Press
            , output = [EventPedal pedal Press]
            }
        , Mealy.Transition {
            from   = StateOnePedal OnePedalPressed{
                         pedalPressed  = pedal
                       , pedalNotified = True
                       }
          , to     = StateNoPedals
          , input  = EventPedal pedal Release
          , output = [EventPedal pedal Release]
          }
        , Mealy.Transition {
            from   = StateOnePedal OnePedalPressed{
                         pedalPressed  = pedal
                       , pedalNotified = False
                       }
          , to     = StateNoPedals
          , input  = EventPedal pedal Release
          , output = []
          }
        ]
      | pedal <- allPedals
      ]

      --
      -- The complicated cases: multiple pedals
      --
      -- The behaviour of the device for simultaneous pedal presses depends on
      -- which pedals are pressed.
      --
      -- Side note: the @(<)@ order is determined by the numerical value of the
      -- pedal (so: 'Pedal10', 'Pedal1', .., 'Pedal9', 'PedalUp', 'PedalDown').
      --
      -- * Case 1: @pedal1 < pedal2@, @pedal2@ released before @pedal1@
      --
      -- > action          | input event
      -- > --------------- | -------------------------
      -- > press   pedal1  | EventPedal pedal1 Press
      -- > press   pedal2  | EventPedal pedal2 Press
      -- > release pedal2  | EventPedal pedal1 Press
      -- > release pedal1  | EventPedal pedal1 Release
      --
      -- Only the third event needs to be corrected here:
      --
      -- > action          | output events
      -- > --------------- | -----------------------------
      -- > press   pedal1  | [ EventPedal pedal1 Press   ]
      -- > press   pedal2  | [ EventPedal pedal2 Press   ]
      -- > release pedal2  | [ EventPedal pedal2 Release ]
      -- > release pedal1  | [ EventPedal pedal1 Release ]
      --
      -- * Case 2: @pedal1 < pedal2@, @pedal1@ released before @pedal2@
      --
      -- > action          | input event
      -- > --------------- | -------------------------
      -- > press   pedal1  | EventPedal pedal1 Press
      -- > press   pedal2  | EventPedal pedal2 Press
      -- > release pedal1  | <nothing>
      -- > release pedal2  | EventPedal pedal2 Release
      --
      -- Since we get no event at all when we release @pedal1@, we can only
      -- correct the event when we release @pedal2@:
      --
      -- > action          | output events
      -- > --------------- | ---------------------------
      -- > press   pedal1  | [ EventPedal pedal1 Press ]
      -- > press   pedal2  | [ EventPedal pedal2 Press ]
      -- > release pedal1  | []
      -- > release pedal2  | [ EventPedal pedal1 Release
      -- >                 | , EventPedal pedal2 Release
      -- >                 | ]
    , concat [
        [ Mealy.Transition {
            from   = StateOnePedal OnePedalPressed{
                         pedalPressed  = pedal1
                       , pedalNotified = notified1
                       }
          , to     = StateTwoPedals TwoPedalsPressed{
                         pedalPressed1  = pedal1
                       , pedalPressed2  = pedal2
                       , pedalNotified1 = notified1
                       }
          , input  = EventPedal pedal2 Press
          , output = [ EventPedal pedal2 Press ]
          }
        , Mealy.Transition {
            from   = StateTwoPedals TwoPedalsPressed{
                          pedalPressed1  = pedal1
                        , pedalPressed2  = pedal2
                        , pedalNotified1 = notified1
                        }
          , to     = StateOnePedal OnePedalPressed{
                          pedalPressed  = pedal1
                        , pedalNotified = notified1
                        }
          , input  = EventPedal pedal1 Press        -- Case 1
          , output = [ EventPedal pedal2 Release ]
          }
        , Mealy.Transition {
            from   = StateTwoPedals TwoPedalsPressed{
                         pedalPressed1  = pedal1
                       , pedalPressed2  = pedal2
                       , pedalNotified1 = notified1
                       }
          , to     = StateNoPedals
          , input  = EventPedal pedal2 Release      -- Case 2
          , output = concat [
                [ EventPedal pedal1 Release | notified1 ]
              , [ EventPedal pedal2 Release ]
              ]
          }
        ]
      | pedal1 <- allPedals
      , pedal2 <- allPedals
      , fromEnum pedal1 < fromEnum pedal2
      , notified1 <- [True, False]
      ]

      -- * Case 3: @pedal1 > pedal2@, @pedal2@ released before @pedal1@.
      --
      -- In this case we get no events for @pedal2@ press or release.
      --
      -- * Case 4: @pedal1 > pedal2@, @pedal1@ released before @pedal2@.
      --
      -- > action         | input event
      -- > -------------- | -------------------------
      -- > press pedal1   | EventPedal pedal1 Press
      -- > press pedal2   | <nothing>
      -- > release pedal1 | EventPedal pedal2 Press
      -- > release pedal2 | EventPedal pedal2 Release
      --
      -- We are lucky that the three events we get in the "two pedals pressed"
      -- state (cases 1, 2 and 4) are all different.
      --
      -- We get no press event for @pedal2@, so the only goal here is to send
      -- the correct events for @pedal1@. After this, we end up in a state which
      -- is /nearly/ indistinguishable from simply having pressed @pedal2@
      -- (cases 1-4), except that we should not be generating any @Release@
      -- events for @pedal2@.
      --
      -- > action         | output events
      -- > -------------- | -----------------------------
      -- > press pedal1   | [ EventPedal pedal1 Press   ]
      -- > press pedal2   | []
      -- > release pedal1 | [ EventPedal pedal1 Release ]
      -- > release pedal2 | []
    , concat [
        [ Mealy.Transition {
             from   = StateOnePedal OnePedalPressed{
                          pedalPressed  = pedal1
                        , pedalNotified = notified1
                        }
           , to     = StateOnePedal OnePedalPressed{
                          pedalPressed  = pedal2
                        , pedalNotified = False
                        }
           , input  = EventPedal pedal2 Press        -- Case 4
           , output = [ EventPedal pedal1 Release ]
           }
        ]
      | pedal1 <- allPedals
      , pedal2 <- allPedals
      , fromEnum pedal1 > fromEnum pedal2
      , notified1 <- [True, False]
      ]
    ]

allPedals :: [Pedal]
allPedals = [minBound .. maxBound]
