{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Simanneal (simanneal, SimannealState, Energy (..), Temperature (..), randomRS, Seed, (^*), energyFloat) where

import Control.Lens (Iso', iso)
import Control.Monad (when)
import Control.Monad.State.Class (MonadState, get, put)
import Control.Monad.Trans.State (State, runState)
import Data.Maybe (fromMaybe)
import System.Random (Random, StdGen, mkStdGen, randomR)

newtype Temperature = Temperature Float deriving (Fractional, Num, Eq, Ord)

newtype Energy = Energy Float deriving (Num, Ord, Eq)

scaleEnergy :: Energy -> Float -> Energy
scaleEnergy (Energy g) x = Energy (g * x)

(^*) :: Energy -> Float -> Energy
(^*) = scaleEnergy

energyFloat :: Iso' Energy Float
energyFloat = iso (\(Energy i) -> i) Energy

data SimannealData d = SimannealData
  { myStdGen :: StdGen,
    bestSolutionEnergy :: Maybe Energy,
    bestSolution :: Maybe d
  }

type SimannealState d = State (SimannealData d)

randomRS :: (MonadState (SimannealData d) m, Random b) => (b, b) -> m b
randomRS range = do
  s <- get
  let (result, newGen) = randomR range (myStdGen s)
  put (s {myStdGen = newGen})
  pure result

type Seed = Int

-- https://medium.com/swlh/how-to-implement-simulated-annealing-algorithm-in-python-ab196c2f56a0
-- https://oleg.fi/gists/posts/2020-06-02-simulated-annealing.html
simanneal ::
  forall s.
  Seed ->
  s ->
  (s -> SimannealState s s) ->
  (s -> Energy) ->
  Temperature ->
  Temperature ->
  Temperature ->
  s
simanneal seed startState chooseNeighbor energy startTemp tempDiff finalTemp =
  let (result, finalState) = runState (simanneal'' startState (energy startState) chooseNeighbor energy startTemp tempDiff finalTemp) (SimannealData (mkStdGen seed) Nothing Nothing)
   in fromMaybe result (bestSolution finalState)

simanneal'' ::
  forall s.
  s ->
  Energy ->
  (s -> SimannealState s s) ->
  (s -> Energy) ->
  Temperature ->
  Temperature ->
  Temperature ->
  SimannealState s s
simanneal'' startState startEnergy chooseNeighbor energy (Temperature startTemp) (Temperature tempDiff) (Temperature finalTemp) = simanneal''' startTemp startState startEnergy
  where
    simanneal''' temp currentState currentEnergy =
      if temp <= finalTemp
        then pure currentState
        else do
          newNeighbor <- chooseNeighbor currentState
          let newNeighborEnergy = energy newNeighbor
              nextTemp = temp - tempDiff
              costDiff = newNeighborEnergy - currentEnergy
          if costDiff <= 0
            then do
              s <- get
              let previousBest = bestSolutionEnergy s
              when (Data.Maybe.fromMaybe True ((> newNeighborEnergy) <$> previousBest)) $
                put (s {bestSolutionEnergy = Just newNeighborEnergy, bestSolution = Just newNeighbor})
              simanneal''' nextTemp newNeighbor newNeighborEnergy
            else do
              r <- randomRS (0.0, 1.0)
              let Energy positiveCosts = negate costDiff
                  scaledCosts = positiveCosts * startTemp
                  decider = exp (negate (scaledCosts / temp))
              if r < decider
                then simanneal''' nextTemp newNeighbor newNeighborEnergy
                else simanneal''' nextTemp currentState currentEnergy
