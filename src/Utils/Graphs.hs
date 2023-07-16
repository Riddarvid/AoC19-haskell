{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Utils.Graphs (bfs, Adjacency) where

import           Control.Monad.Except (Except, MonadError (throwError),
                                       runExcept)
import           Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import           Control.Monad.State  (MonadState, StateT (runStateT), gets,
                                       modify)
import           Data.Foldable        (toList)
import           Data.Hashable        (Hashable)
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Lazy    as HM
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HS

type Path n = [n]

type Adjacency t n = n -> t n

data BfsEnv t n = BFSE {
  bfsTarget    :: n,
  bfsAdjacency :: Adjacency t n
}

mkEnv :: n -> Adjacency t n -> BfsEnv t n
mkEnv target adjacency = BFSE {bfsTarget = target, bfsAdjacency = adjacency}

data BfsState n = BFSS {
  bfsPreMap    :: HashMap n (Maybe n),
  bfsLastLayer :: HashSet n
}

mkState :: (Hashable n) => n -> BfsState n
mkState start = BFSS {bfsPreMap = HM.singleton start Nothing, bfsLastLayer = HS.singleton start}

newtype BfsMonad t n a = BFSM (ReaderT (BfsEnv t n) (StateT (BfsState n) (Except ())) a)
  deriving (Functor, Applicative, Monad,
  MonadReader (BfsEnv t n), MonadState (BfsState n), MonadError ())

runBFSM :: BfsMonad t n a -> BfsEnv t n -> BfsState n -> Maybe (a, BfsState n)
runBFSM (BFSM m) env st = case runExcept (runStateT (runReaderT m env) st) of
  Left ()   -> Nothing
  Right res -> Just res

evalBFSM :: BfsMonad t n a -> BfsEnv t n -> BfsState n -> Maybe a
evalBFSM m env st = fst <$> runBFSM m env st

bfs :: (Hashable n, Foldable t) => n -> n -> Adjacency t n -> Maybe (Path n)
bfs start target adjecency = evalBFSM explore (mkEnv target adjecency) (mkState start)

explore :: (Hashable n, Foldable t) => BfsMonad t n (Path n)
explore = do
  done <- isDone
  if done
    then constructPath
    else do
      lastLayer <- gets bfsLastLayer
      if HS.size lastLayer == 0
        then throwError ()
        else exploreNextLayer >> explore

constructPath :: Hashable n => BfsMonad t n (Path n)
constructPath = do
  target <- asks bfsTarget
  preMap <- gets bfsPreMap
  constructPath' preMap target []

constructPath' :: Hashable n => HashMap n (Maybe n) -> n -> Path n -> BfsMonad t n (Path n)
constructPath' preMap node path = case HM.lookup node preMap of
  Nothing -> throwError ()
  Just pre -> let path' = node : path in case pre of
    Nothing   -> return path'
    Just pre' -> constructPath' preMap pre' path'

-- We are done if the last layer contains the target node.
isDone :: (Hashable n) => BfsMonad t n Bool
isDone = do
  lastLayer <- gets bfsLastLayer
  target <- asks bfsTarget
  return $ HS.member target lastLayer

exploreNextLayer :: (Hashable n, Foldable t) => BfsMonad t n ()
exploreNextLayer = do
  lastLayer <- gets bfsLastLayer
  currentLayer <- mconcat <$> mapM exploreNeighbors (HS.toList lastLayer)
  modify (\s -> s{bfsLastLayer = currentLayer})

exploreNeighbors :: (Hashable n, Foldable t) => n -> BfsMonad t n (HashSet n)
exploreNeighbors node = do
  adjacency <- asks bfsAdjacency
  preMap <- gets bfsPreMap
  let neighbors = adjacency node
  let neighbors' = filter (\x -> not $ HM.member x preMap) $ toList neighbors
  mapM_ (addPredecessor node) neighbors'
  return $ HS.fromList neighbors'

addPredecessor :: (Hashable n) => n -> n -> BfsMonad t n ()
addPredecessor pre node = modify (\s -> s{bfsPreMap = HM.insert node (Just pre) (bfsPreMap s)})
