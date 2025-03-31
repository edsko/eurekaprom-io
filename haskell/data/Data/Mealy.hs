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
  , mapState
  , mapInputOutput
    -- * Encoding state
  , encodeState
    -- * Execution
  , step
  , ExecEnv(..)
  , exec
  ) where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | Mealy machine
data Mealy s i o = Mealy {
      transitions :: Map s (Map i (s, o))
    }

{-------------------------------------------------------------------------------
  Transitions
-------------------------------------------------------------------------------}

data Transition s i o = Transition {
      from :: (s, i)
    , to   :: (s, o)
    }

fromTransitions :: forall s i o.
      (Ord s, Ord i)
   => [Transition s i o] -> Mealy s i o
fromTransitions =
    Mealy . Map.unionsWith Map.union . map singleton
  where
    singleton :: Transition s i o -> Map s (Map i (s, o))
    singleton Transition{from = (s, i), to} =
        Map.singleton s $ Map.singleton i to

toTransitions :: forall s i o. Mealy s i o -> [Transition s i o]
toTransitions =
    concatMap aux . map (second Map.toList) . Map.toList . transitions
  where
    aux :: (s, [(i, (s, o))]) -> [Transition s i o]
    aux (s, fromS) = [Transition{from = (s, i), to} | (i, to) <- fromS]

{-------------------------------------------------------------------------------
  Combinators
-------------------------------------------------------------------------------}

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
    aux Transition{from, to} = Transition{
          from   = first f from
        , to     = first f to
        }

mapInputOutput :: forall f s i o.
     (Ord s, Ord (f i))
  => (forall x. x -> f x)
  -> Mealy s i o -> Mealy s (f i) (f o)
mapInputOutput f =
      fromTransitions
    . map aux
    . toTransitions
  where
    aux :: Transition s i o -> Transition s (f i) (f o)
    aux Transition{from, to} = Transition{
          from = second f from
        , to   = second f to
        }

{-------------------------------------------------------------------------------
  Encoding states
-------------------------------------------------------------------------------}

allStates :: Ord s => Mealy s i o -> Set s
allStates = Set.fromList . concatMap aux . toTransitions
  where
    aux :: Transition s i o -> [s]
    aux Transition{from, to} = [fst from, fst to]

-- | Assign unique 'Int' to every state
encodeState :: forall s i o.
     (Ord s, Ord i)
  => Mealy s i o -> Mealy Key i o
encodeState machine =
    mapState (\s -> toKey $ states Map.! s) machine
  where
    states :: Map s Int
    states = Map.fromList $ zip (Set.toList $ allStates machine) [0..]

    toKey :: Int -> Key
    toKey = Key.fromString . show

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

step :: (Ord i, Ord s) => Mealy s i o -> (s, i) -> Maybe (s, o)
step machine (s, i) = Map.lookup s (transitions machine) >>= Map.lookup i

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
          case step machine (s, i) of
            Nothing      -> unrecognized env s i >> go s
            Just (s', o) -> processOutput env o  >> go s'

{-------------------------------------------------------------------------------
  JSON
-------------------------------------------------------------------------------}

instance (ToJSON i, ToJSON o) => ToJSON (Mealy Key i o) where
  toJSON Mealy{transitions} = object $ map aux (Map.toList transitions)
    where
      aux :: (Key, Map i (Key, o)) -> Pair
      aux (s, fromS) = s .= Map.toList fromS
