module RandomUtils (RandomM, randomRM, randomListElementNE, evalRandomM, mkStdGen) where

import Control.Monad.State.Strict (State, evalState, get, put)
import qualified Data.List.NonEmpty as NE
import System.Random (Random, RandomGen, mkStdGen, randomR)

type RandomM g = State g

randomRM :: (RandomGen g, Random a) => (a, a) -> RandomM g a
randomRM r = do
  g <- get
  let (a, newG) = randomR r g
  put newG
  pure a

randomListElementNE :: RandomGen g => NE.NonEmpty a -> RandomM g a
randomListElementNE xs = do
  randomIndex <- randomRM (0, NE.length xs - 1)
  pure (xs NE.!! randomIndex)

evalRandomM :: RandomGen g => g -> RandomM g a -> a
evalRandomM g a = evalState a g
