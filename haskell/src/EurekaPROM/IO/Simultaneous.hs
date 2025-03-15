-- | Support simultaneous pedal presses
--
-- Intended for qualified import.
--
-- > import EurekaPROM.IO.Simultaneous (simultaneous)
-- > import EurekaPROM.IO.Simultaneous qualified as Simultaneous
module EurekaPROM.IO.Simultaneous (
    DeviceState(..)
  , initDeviceState
  , simultaneous
  ) where

import Data.Mealy (Mealy)
import Data.Mealy qualified as Mealy

import EurekaPROM.IO.Input

{-------------------------------------------------------------------------------
  State
-------------------------------------------------------------------------------}

data DeviceState =
    NoPedalsPressed
  | OnePedalPressed Pedal
  | TwoPedalsPressed Pedal Pedal -- ^ In chronological order
  deriving stock (Show, Eq, Ord)

initDeviceState :: DeviceState
initDeviceState = NoPedalsPressed

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
              from   = NoPedalsPressed
            , to     = OnePedalPressed pedal
            , input  = EventPedal pedal Press
            , output = [EventPedal pedal Press]
            }
        , Mealy.Transition {
            from   = OnePedalPressed pedal
          , to     = NoPedalsPressed
          , input  = EventPedal pedal Release
          , output = [EventPedal pedal Release]
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
      -- * Case 1: @pedal1 < pedal2@, @pedal2@ released before @pedal1@
      --
      -- Note the @(<)@ order here is determined by the numerical value of the
      -- pedal (so: 'Pedal10', 'Pedal1', .., 'Pedal9', 'PedalUp', 'PedalDown').
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
      -- > --------------- | -------------------------
      -- > press   pedal1  | [ EventPedal pedal1 Press   ]
      -- > press   pedal2  | [ EventPedal pedal2 Press   ]
      -- > release pedal2  | [ EventPedal pedal2 Press   ]
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
      -- > --------------- | -------------------------
      -- > press   pedal1  | [ EventPedal pedal1 Press ]
      -- > press   pedal2  | [ EventPedal pedal2 Press ]
      -- > release pedal1  | []
      -- > release pedal2  | [ EventPedal pedal1 Release
      -- >                 | , EventPedal pedal2 Release
      -- >                 | ]
      --
      -- * Case 3: @pedal1 > pedal2@.
      --
      -- In this case we get no events for @pedal2@ press or release.
    , concat [
        [ Mealy.Transition {
            from   = OnePedalPressed pedal1
          , to     = TwoPedalsPressed pedal1 pedal2
          , input  = EventPedal pedal2 Press
          , output = [ EventPedal pedal2 Press ]
          }
        , Mealy.Transition {
            from   = TwoPedalsPressed pedal1 pedal2
          , to     = OnePedalPressed pedal1
          , input  = EventPedal pedal1 Press
          , output = [ EventPedal pedal2 Release ]
          }
        , Mealy.Transition {
            from   = TwoPedalsPressed pedal1 pedal2
          , to     = NoPedalsPressed
          , input  = EventPedal pedal2 Release
          , output = [ EventPedal pedal1 Release
                     , EventPedal pedal2 Release
                     ]
          }
        ]
      | pedal1 <- allPedals
      , pedal2 <- allPedals
      , fromEnum pedal1 < fromEnum pedal2
      ]
    ]

allPedals :: [Pedal]
allPedals = [minBound .. maxBound]
