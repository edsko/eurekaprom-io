module Main (main) where

import Data.Bifunctor
import Data.Proxy
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.QuickCheck

import Test.QuickCheck.StateModel
import Test.QuickCheck.StateModel.Lockstep
import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep
import Test.QuickCheck.StateModel.Lockstep.Run      qualified as Lockstep

import EurekaPROM.IO.Input

import Test.Lockstep.SUT (SystemMonad, SystemPort)
import Test.Lockstep.SUT qualified as SUT
import Test.Lockstep.Model

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

instance StateModel (Lockstep Model) where
  data Action (Lockstep Model) a where
    ActPedal :: PedalEvent -> Action (Lockstep Model) [PedalEvent]

  initialState    = Lockstep.initialState initModel
  nextState       = Lockstep.nextState
  arbitraryAction = Lockstep.arbitraryAction
  shrinkAction    = Lockstep.shrinkAction

  precondition model (ActPedal action) = and [
        Lockstep.precondition model (ActPedal action)
      , canPerform (getModel model) action
      ]

instance InLockstep Model where
  data ModelValue Model a where
    ModelEvents :: [PedalEvent] -> ModelValue Model [PedalEvent]
  data Observable Model a where
    ObservableEvents :: [PedalEvent] -> Observable Model [PedalEvent]

  usedVars _action =
      []
  observeModel (ModelEvents es) =
      ObservableEvents es
  modelNextState (ActPedal action) _ctxt state =
      first ModelEvents $ execute action state
  arbitraryWithVars _ctxt state = elements $
      map (Some . ActPedal) $ possibleActions state

instance RunModel (Lockstep Model) SystemMonad where
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @SystemMonad)

  perform model (ActPedal action) _ctxt = do
      SUT.showInstruction action
      if visible (getModel model) (pedalEventPedal action)
        then SUT.getEvents
        else SUT.delay >> return []

instance RunLockstep Model SystemMonad where
  observeReal _ (ActPedal _) res = ObservableEvents res

deriving stock instance Show (Action (Lockstep Model) a)
deriving stock instance Eq   (Action (Lockstep Model) a)

deriving stock instance Show (ModelValue Model a)
deriving stock instance Show (Observable Model a)
deriving stock instance Eq   (Observable Model a)

{-------------------------------------------------------------------------------
  Application top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMainWithIngredients ingredients $
    -- We need the MIDI port to listen to
    askOption $ \mPortSpec ->
    -- Disable shrinking (too slow due to human involvement in test execution)
    localOption (QuickCheckMaxShrinks 0) $
    -- A few tests are sufficient (each test is itself many tests)
    localOption (QuickCheckTests 2) $
    -- Setup the actual lockstep test
      testGroup "test-mealy" [
          testProperty "lockstep" $
            Lockstep.runActionsBracket
              (Proxy @Model)
              (SUT.initialize mPortSpec)
              SUT.terminate
              SUT.run
        ]
  where
    ingredients :: [Ingredient]
    ingredients =
          includingOptions [Option (Proxy @SystemPort)]
        : defaultIngredients
