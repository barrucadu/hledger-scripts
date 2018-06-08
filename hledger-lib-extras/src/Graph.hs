module Graph where

import           Data.Map (Map)
import qualified Data.Map as M

-- | A directed graph with nodes of type @n@ and edge labels of type
-- @l@.  Parallel edges are not allowed.
type Graph n l = Map n (Map n l)

-- | A graph with no nodes or edges.
empty :: Ord n => Graph n l
empty = M.empty

-- | Add a node to a graph, if not already present.
addNode :: Ord n => n -> Graph n l -> Graph n l
addNode n = M.insertWith (\_ old -> old) n M.empty

-- | Add an edge to a graph, combining edges if they exist.
--
-- If the source node doesn't exist, does not change the graph.
addEdge
  :: Ord n
  => (l -> l -> l)  -- ^ Function to combine edge labels.
  -> n  -- ^ Source node.
  -> n  -- ^ Target node.
  -> l  -- ^ New label.
  -> Graph n l
  -> Graph n l
addEdge combine from to label graph = case M.lookup from graph of
  Just edges ->
    let edges' = M.insertWith combine to label edges
    in  M.insert from edges' graph
  Nothing -> graph

-- | Take the reflexive closure by adding edges with the given label
-- where missing.
reflexiveClosure :: Ord n => l -> Graph n l -> Graph n l
reflexiveClosure label graph = foldr
  (.)
  id
  [ addEdge (\_ old -> old) nA nA label | nA <- M.keys graph ]
  graph

-- | Take the symmetric closure by adding new edges, transforming
-- existing labels.
symmetricClosure :: Ord n => (l -> l) -> Graph n l -> Graph n l
symmetricClosure mk graph = foldr
  (.)
  id
  [ addEdge (\_ old -> old) nB nA (mk lAB)
  | (nA, edges) <- M.assocs graph
  , (nB, lAB  ) <- M.assocs edges
  ]
  graph

-- | Take the transitive closure by adding new edges, combining
-- existing labels.
transitiveClosure :: (Ord n, Eq l) => (l -> l -> l) -> Graph n l -> Graph n l
transitiveClosure combine = fixEq step
 where
  fixEq f = find . iterate f
   where
    find (a1:a2:as) | a1 == a2  = a1
                    | otherwise = find (a2 : as)
    find _ = error "unreachable"

  step graph = foldr
    (.)
    id
    [ addEdge (\_ old -> old) nA nC (combine lAB lBC)
    | (nA, edges) <- M.assocs graph
    , (nB, lAB  ) <- M.assocs edges
    , (nC, lBC  ) <- M.assocs (M.findWithDefault M.empty nB graph)
    ]
    graph
