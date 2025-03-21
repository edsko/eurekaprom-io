{-# LANGUAGE OverloadedStrings #-}

-- | Mealy machines
--
-- Intended for qualified import.
--
-- > import Data.Mealy (Mealy)
-- > import Data.Mealy qualified as Mealy
module Data.Mealy (
    Mealy -- opaque
    -- * Transitions
  , Transition(..)
  , fromTransitions
  , toTransitions
    -- * Combinators
  , wrap
  , mapState
    -- * Encoding state
  , StateNum(..)
  , encodeState
    -- * Execution
  , ExecEnv(..)
  , exec
  ) where

import Data.Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mealy machine
data Mealy s i o = Mealy {
      transitions :: Map (s, i) (s, o)
    }

{-------------------------------------------------------------------------------
  Transitions
-------------------------------------------------------------------------------}

data Transition s i o = Transition {
      from   :: s
    , input  :: i
    , to     :: s
    , output :: o
    }

fromTransitions :: forall s i o.
      (Ord s, Ord i)
   => [Transition s i o] -> Mealy s i o
fromTransitions = Mealy . Map.fromList . map aux
  where
    aux :: Transition s i o -> ((s, i), (s, o))
    aux Transition{from, input, to, output} = (
          (from, input)
        , (to, output)
        )

toTransitions :: forall s i o. Mealy s i o -> [Transition s i o]
toTransitions = map aux . Map.toList . transitions
  where
    aux :: ((s, i), (s, o)) -> Transition s i o
    aux ((from, input), (to, output)) = Transition{from, input, to, output}

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

wrap :: forall f s i o.
     (Ord (f s), Ord (f i))
  => (forall x. x -> f x)
  -> Mealy s i o -> Mealy (f s) (f i) (f o)
wrap f =
      fromTransitions
    . map aux
    . toTransitions
  where
    aux :: Transition s i o -> Transition (f s) (f i) (f o)
    aux Transition{from, input, to, output} = Transition{
          from   = f from
        , input  = f input
        , to     = f to
        , output = f output
        }

mapState :: forall s s' i o.
     (Ord s', Ord i)
  => (s -> s')
  -> Mealy s i o -> Mealy s' i o
mapState f =
      fromTransitions
    . map aux
    . toTransitions
  where
    aux :: Transition s i o -> Transition s' i o
    aux Transition{from, input, to, output} = Transition{
          from   = f from
        , to     = f to
        , input
        , output
        }

{-------------------------------------------------------------------------------
  Encoding states
-------------------------------------------------------------------------------}

-- | Numerical state
--
-- See 'encodeState'
newtype StateNum = StateNum Int
  deriving stock (Show, Eq, Ord)
  deriving newtype (ToJSON)

allStates :: Ord s => Mealy s i o -> Set s
allStates = Set.fromList . concatMap aux . toTransitions
  where
    aux :: Transition s i o -> [s]
    aux Transition{from, to} = [from, to]

-- | Assign unique 'Int' to every state
encodeState :: forall s i o.
     (Ord s, Ord i)
  => Mealy s i o -> Mealy StateNum i o
encodeState machine =
    mapState (\s -> StateNum $ states Map.! s) machine
  where
    states :: Map s Int
    states = Map.fromList $ zip (Set.toList $ allStates machine) [0..]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

-- | Execution environment for Mealy machine
data ExecEnv m s i o = ExecEnv {
      produceInput  :: m i
    , processOutput :: o -> m ()
    , unrecognized  :: s -> i -> m ()
    , initialState  :: s
    , finalState    :: s -> Bool
    }

exec :: forall m s i o.
     (Monad m, Ord i, Ord s)
  => Mealy s i o -> ExecEnv m s i o -> m ()
exec machine env =
    go $ initialState env
  where
    go :: s -> m ()
    go s = do
        if finalState env s then
          return ()
        else do
          i <- produceInput env
          case Map.lookup (s, i) (transitions machine) of
            Nothing      -> unrecognized env s i >> go s
            Just (s', o) -> processOutput env o      >> go s'

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance (ToJSON s, ToJSON i, ToJSON o) => ToJSON (Mealy s i o) where
  toJSON = toJSON . toTransitions

instance (ToJSON s, ToJSON i, ToJSON o) => ToJSON (Transition s i o) where
  toJSON Transition{from, input, to, output} = object [
        "from"   .= from
      , "input"  .= input
      , "to"     .= to
      , "output" .= output
      ]
