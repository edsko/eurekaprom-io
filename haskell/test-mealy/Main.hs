module Main (main) where

import Control.Monad.IO.Class
import Data.Proxy
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.QuickCheck

import Test.QuickCheck.StateModel
import Test.QuickCheck.StateModel.Lockstep
import Test.QuickCheck.StateModel.Lockstep.Defaults qualified as Lockstep
import Test.QuickCheck.StateModel.Lockstep.Run      qualified as Lockstep

import Test.Mealy.SUT (SystemMonad, SystemPort)
import Test.Mealy.SUT qualified as SUT

{-------------------------------------------------------------------------------
  Model
-------------------------------------------------------------------------------}

data Model =
    PressedZero
  | PressedOne SUT.Pedal
  deriving stock (Show)

data PedalAction =
    Press   SUT.Pedal
  | Release SUT.Pedal
  deriving stock (Show, Eq)

instance StateModel (Lockstep Model) where
  data Action (Lockstep Model) a where
    ActionPedal :: PedalAction -> Action (Lockstep Model) [SUT.Event]

  initialState    = Lockstep.initialState PressedZero
  nextState       = Lockstep.nextState
  arbitraryAction = Lockstep.arbitraryAction
  shrinkAction    = Lockstep.shrinkAction

  precondition model (ActionPedal pedalAction) = and [
        Lockstep.precondition model (ActionPedal pedalAction)
      , case (getModel model, pedalAction) of
          (PressedZero, Press _) ->
            True
          (PressedZero, Release _) ->
            False
          (PressedOne _pedal, Press _) ->
            False
          (PressedOne pedal, Release pedal') ->
            pedal == pedal'
      ]

instance InLockstep Model where
  data ModelValue Model a where
    ModelEvents :: [SUT.Event] -> ModelValue Model [SUT.Event]
  data Observable Model a where
    ObservableEvents :: [SUT.Event] -> Observable Model [SUT.Event]

  usedVars _action = []
  observeModel (ModelEvents es) = ObservableEvents es

  modelNextState (ActionPedal pedalAction) _ctxt state =
      case (state, pedalAction) of
        (PressedZero, Press pressed) -> (
            ModelEvents [SUT.EventPedal pressed SUT.Press]
          , PressedOne pressed
          )
        (PressedOne pedal, Release released) | pedal == released -> (
            ModelEvents [SUT.EventPedal released SUT.Release]
          , PressedZero
          )
        _otherwise ->
          error $ "unexpected action: " ++ show (state, pedalAction)

  arbitraryWithVars _ctxt state =
      case state of
        PressedZero ->
          Some . ActionPedal . Press <$> arbitrary
        PressedOne pedal ->
          return $ Some . ActionPedal . Release $ pedal

instance RunModel (Lockstep Model) SystemMonad where
  postcondition = Lockstep.postcondition
  monitoring    = Lockstep.monitoring (Proxy @SystemMonad)

  perform _state (ActionPedal pedalAction) _ctxt = do
      liftIO $ print pedalAction
      SUT.getEvents

instance RunLockstep Model SystemMonad where
  observeReal _ (ActionPedal _) res = ObservableEvents res

deriving stock instance Show (Action (Lockstep Model) a)
deriving stock instance Eq   (Action (Lockstep Model) a)

deriving stock instance Show (ModelValue Model a)
deriving stock instance Show (Observable Model a)
deriving stock instance Eq   (Observable Model a)

{-------------------------------------------------------------------------------
  Application top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMainWithIngredients ingredients $ askOption $ \mPortSpec ->
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
