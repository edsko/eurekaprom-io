-- | Model
--
-- Intended for unqualified import.
module Test.Mealy.Model (
    -- * Model
    Model(..)
  , initModel
  , visible
    -- * Actions
  , PedalAction(..)
  , canPerform
  , possibleActions
  , execute
  ) where

import Data.Map (Map)
import Data.Map qualified as Map

import EurekaPROM.IO.Input

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

data Model = Model {
      -- | Currently pressed pedals
      --
      -- We record if we received the corresponding 'Press' events.
      pressed :: Map Pedal Bool

      -- | Delayed 'Release' event, if any
    , delayed :: [Event]
    }
  deriving stock (Show)

initModel :: Model
initModel = Model {
      pressed = Map.empty
    , delayed = []
    }

visible :: Model -> Pedal -> Bool
visible Model{pressed} pedal = all (<= pedal) (Map.keys pressed)

{-------------------------------------------------------------------------------
  Actions
-------------------------------------------------------------------------------}

data PedalAction =
    ActPress   { pedalOf :: Pedal }
  | ActRelease { pedalOf :: Pedal }
  deriving stock (Show, Eq)

canPerform :: Model -> PedalAction -> Bool
canPerform Model{pressed} (ActPress pedal) =
    Map.notMember pedal pressed && Map.size pressed <= 1
canPerform Model{pressed} (ActRelease pedal) =
    Map.member pedal pressed

possibleActions :: Model -> [PedalAction]
possibleActions pressed = filter (canPerform pressed) . concat $ [
      [ActPress pedal, ActRelease pedal]
    | pedal <- [minBound .. maxBound]
    ]

execute :: PedalAction -> Model -> ([Event], Model)
execute pedalAction state@Model{pressed, delayed} =
    case (pedalAction, visible state (pedalOf pedalAction)) of
      (ActPress pedal, True) -> (
           delayed ++ new
         , Model{ pressed = Map.insert pedal True pressed
                , delayed = []
                }
         )
      (ActPress pedal, False) -> (
           []
         , Model{ pressed = Map.insert pedal False pressed
                , delayed = delayed
                }
         )
      (ActRelease pedal, True) -> (
           delayed ++ new
         , Model{ pressed = Map.delete pedal pressed
                , delayed = []
                }
         )
      (ActRelease pedal, False) -> (
           []
         , Model{ pressed = Map.delete pedal pressed
                , delayed = delayed ++ new
                }
         )
  where
    new :: [Event]
    new =
        case pedalAction of
          ActPress   pedal -> [EventPedal pedal Press]
          ActRelease pedal -> [EventPedal pedal Release | pressed Map.! pedal]
