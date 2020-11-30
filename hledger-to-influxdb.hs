#!/usr/bin/env stack
{- stack script
  --nix --no-nix-pure --nix-packages zlib
  --resolver lts-16.16
  --package containers,Decimal,hledger-lib,influxdb,text,time
-}

{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Arrow      ((***), second)
import           Data.Decimal       (Decimal)
import           Data.Foldable      (for_)
import           Data.Function      (on)
import           Data.List          (foldl', groupBy, inits, mapAccumL, nub, sortOn)
import qualified Data.Map           as M
import           Data.Maybe         (mapMaybe)
import           Data.String        (IsString, fromString)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day, toGregorian, fromGregorian, gregorianMonthLength)
import           Data.Time.Clock    (UTCTime (..))
import           Database.InfluxDB  as I
import           Hledger.Data.Types as H
import           Hledger.Read       as H

main :: IO ()
main = do
  journal <- H.defaultJournal
  let measurements = toMeasurements (H.jpricedirectives journal) (H.jtxns journal)
  for_ measurements $ \(name, ms) -> do
    putStrLn ("Writing " <> T.unpack name <> " (" <> show (length ms) <> " measurements)")
    for_ (chunksOf 300 ms) (I.writeBatch (I.writeParams "finance"))
  putStrLn "Done"

toMeasurements
  :: [H.PriceDirective] -> [H.Transaction] -> [(T.Text, [I.Line UTCTime])]
toMeasurements prices txns =
  [ measurements nvalue "normal" dailies
  , measurements cvalue "cost"   dailies
  , measurements mvalue "market" dailies
  , measurements (periodToInflux marketValues) "period" periods
  , measurements countToInflux "count"  txns
  , measurements priceToInflux "market" prices
  ]
 where
  marketValues = marketValue (buildPrices prices)

  nvalue = balancesToInflux (const normalValue)
  cvalue = balancesToInflux (const costValue)
  mvalue = toInflux accountKey
                    marketValues
                    (toDeltas (const normalValue))

  dailies = mapMaybe squish $ groupBy ((==) `on` H.tdate) txns
  squish ts@(t:_) = Just t { H.tdescription = "aggregate"
                           , H.tpostings    = concatMap H.tpostings ts
                           }
  squish _ = Nothing

  periods = groupBy ((==) `on` (periodOf . H.tdate)) txns

  measurements toL name xs =
    let go start = mapAccumL
          (toL (fromText name))
          start
          xs
        initialAccounts = M.map (const 0) (fst (go M.empty))
    in  (name, snd (go initialAccounts))

  balancesToInflux = toInflux accountKey (\_ ac q -> [(ac, q)]) . toDeltas
  countToInflux    = toInflux fromText (\_ ac q -> [(ac, q)]) $ \txn ->
    [ (showStatus status, if H.tstatus txn == status then 1 else 0)
    | status <- [minBound .. maxBound]
    ]

toInflux
  :: Ord k
  => (k -> I.Key)
  -> (Day -> k -> Decimal -> [(k, Decimal)])
  -> (H.Transaction -> [(k, Decimal)])
  -> I.Measurement
  -> M.Map k Decimal
  -> H.Transaction
  -> (M.Map k Decimal, I.Line UTCTime)
toInflux toKey transform deltaf key state txn =
  (state', Line key M.empty fields (Just time))
 where
  time = UTCTime (H.tdate txn) 0
  fields = toFields state'

  state'  = M.unionWith (+) state deltas
  deltas  = M.fromList (deltaf txn)

  toFields =
    M.fromList
      . map (toKey *** (I.FieldFloat . doub))
      . sumSame
      . concatMap (uncurry (transform (H.tdate txn)))
      . M.toList

priceToInflux
  :: a
  -> M.Map (H.CommoditySymbol, H.CommoditySymbol) Decimal
  -> H.PriceDirective
  -> ( M.Map (H.CommoditySymbol, H.CommoditySymbol) Decimal
     , I.Line UTCTime
     )
priceToInflux _ state (H.PriceDirective day c (H.Amount c' q' _ _ _)) =
  (state', Line "market" M.empty fields (Just (UTCTime day 0)))
 where
  fields   = toFields state'
  state'   = M.insert (c, c') q' state
  toFields = M.fromList . map (priceKey *** (I.FieldFloat . doub)) . M.toList

-- note: this doesn't count (eg) the "income" generated by taking out
-- a loan, or the "expense" incurred in paying a part of it back; it's
-- purely transactions which touch `income:` and `expenses:`
periodToInflux
  :: (Day -> (AccountName, CommoditySymbol) -> Decimal -> [((AccountName, CommoditySymbol), Decimal)])
  -> a
  -> s
  -> [H.Transaction]
  -> ( s
     , I.Line UTCTime
     )
periodToInflux marketValues _ s txns@(t:_) =
  (s, Line "period" M.empty fields (Just time))
 where
   time = UTCTime (periodOf (H.tdate t)) 0
   fields = toFields $ foldl' toPeriodDelta M.empty txns
   toFields = M.fromList . map (accountKey *** (I.FieldFloat . doub)) . M.toList

   toPeriodDelta :: M.Map (H.AccountName, H.CommoditySymbol) Decimal -> H.Transaction -> M.Map (H.AccountName, H.CommoditySymbol) Decimal
   toPeriodDelta acc txn =
     let deltas :: [((AccountName, CommoditySymbol), Decimal)]
         deltas = concatMap (\(ac,q) -> marketValues (H.tdate txn) ac q) (toDeltas (const normalValue) txn)
         incomeDeltas = deltasFor "income" deltas
         allAssetsDeltas = deltasFor "assets" deltas
         allExpensesDeltas = deltasFor "expenses" deltas
         grossExpensesDeltas = deltasFor "expenses:gross" deltas
         -- zero assetsDeltas if incomeDeltas are missing (i.e. only count income)
         assetsDeltas = if null incomeDeltas then [] else allAssetsDeltas
         -- subtract expenses:gross from expenses (i.e. only count post-tax expenses)
         expensesDeltas = sumSame (allExpensesDeltas ++ map (\((_,c),v) -> (("expenses",c),-v)) grossExpensesDeltas)
     in M.unionWith (+) acc . M.fromList $ assetsDeltas ++ expensesDeltas

   deltasFor a0 = filter $ \((a,_),_) -> a == a0
periodToInflux _ _ _ [] = error "empty period"

toDeltas
  :: (Day -> H.MixedAmount -> [(H.CommoditySymbol, Decimal)])
  -> H.Transaction
  -> [((H.AccountName, H.CommoditySymbol), Decimal)]
toDeltas value txn =
  let postings = concatMap explodeAccount (H.tpostings txn)
      accounts = nub (map H.paccount postings)
  in  [ ((a, cur), val)
      | a <- accounts
      , let ps = filter ((== a) . H.paccount) postings
      , (cur, val) <- sumSame (concatMap (value (tdate txn) . H.pamount) ps)
      ]

showStatus :: H.Status -> T.Text
showStatus H.Cleared  = "cleared"
showStatus H.Pending  = "pending"
showStatus H.Unmarked = "uncleared"

accountKey :: (H.AccountName, H.CommoditySymbol) -> I.Key
accountKey (a, c) = fromText $ a <> "[" <> c <> "]"

priceKey :: (H.CommoditySymbol, H.CommoditySymbol) -> I.Key
priceKey (c, c') = fromText $ c <> "[" <> c' <> "]"

-------------------------------------------------------------------------------
-- * Amounts

-- | The \"normal\" value of an amount: the main commodity.
normalValue :: H.MixedAmount -> [(H.CommoditySymbol, Decimal)]
normalValue (H.Mixed amounts) = map go amounts
  where go (H.Amount c q _ _ _) = (c, q)

-- | The \"cost\" value of an amount: the bit after the \"@\" or
-- \"@@\" (if given), or the normal amount otherwise.
costValue :: H.MixedAmount -> [(H.CommoditySymbol, Decimal)]
costValue (H.Mixed amounts) = map go amounts
 where
  go (H.Amount _ q _ _ (Just (H.UnitPrice  a))) = second (* q) (go a)
  go (H.Amount _ q _ _ (Just (H.TotalPrice a))) = second (* signum q) (go a)
  go (H.Amount c q _ _ _) = (c, q)

-- | The \"market\" value of an amount, as of a given day.
--
-- Unlike 'normalValue' and 'costValue', this takes a single commodity
-- rather than a 'H.MixedAmount'.
marketValue
  :: [(Day, Graph H.CommoditySymbol Decimal)]
  -> Day
  -> (H.AccountName, H.CommoditySymbol)
  -> Decimal
  -> [((H.AccountName, H.CommoditySymbol), Decimal)]
marketValue prices day (a, c) q =
  [ ((a, c'), factor * q) | (c', factor) <- factors ]
 where
  factors = case map snd . dropWhile ((> day) . fst) $ prices of
    (g:_) -> findFactors c g
    []    -> [(c, 1)]

-------------------------------------------------------------------------------
-- * Exchange rates

-- | Build a set of exchange rate graphs.
buildPrices :: [H.PriceDirective] -> [(Day, Graph H.CommoditySymbol Decimal)]
buildPrices prices0 =
  reverse . map (second close) . graphs empty . sortOn fst $ prices
 where
  prices = [ (d, (c, c', q)) | H.PriceDirective d c (H.Amount c' q _ _ _) <- prices0 ]

  graphs g ((day, (c, c', q)):rest) =
    let g' = addEdge const c c' q . addNode c' . addNode c $ g
    in  case rest of
          ((day', _):_) | day == day' -> graphs g' rest
          _                           -> (day, g') : graphs g' rest
  graphs _ [] = []

  close =
    transitiveClosure (*) . symmetricClosure (1 /) . reflexiveClosure 1

-- | Find all the exchange rates for a commodity.
findFactors
  :: H.CommoditySymbol
  -> Graph H.CommoditySymbol Decimal
  -> [(H.CommoditySymbol, Decimal)]
findFactors c = M.assocs . M.findWithDefault (M.singleton c 1) c

-------------------------------------------------------------------------------
-- * Miscellaneous

-- | Explode one posting into one posting per account.
--
-- eg, a posting involving "assets:cash" produces two postings, one
-- involving "assets:cash" and the other involving "assests".
explodeAccount :: H.Posting -> [H.Posting]
explodeAccount p =
  [ p { H.paccount = a }
  | a <- tail . map (T.intercalate ":") . inits . T.splitOn ":" $ H.paccount p
  ]

-- | Take a list of pairs, group by first element, and sum second
-- elements.  The pairs in the result list all have unique first
-- elements.
sumSame :: (Ord k, Num v) => [(k, v)] -> [(k, v)]
sumSame = go . sortOn fst
 where
  go ((k1, v1):(k2, v2):rest) | k1 == k2  = go ((k1, v1 + v2) : rest)
                              | otherwise = (k1, v1) : go ((k2, v2) : rest)
  go xs = xs

-- | Convert a 'Decimal' to a 'Double'.
doub :: Decimal -> Double
doub = fromRational . toRational

-- | Convert a @Text@ to a string-like thing.
fromText :: IsString s => T.Text -> s
fromText = fromString . T.unpack

-- | Split a list unto chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Get the period date of a 'Day'
periodOf :: Day -> Day
periodOf day =
  let (y, m, _) = toGregorian day
  in fromGregorian y m (gregorianMonthLength y m)

-------------------------------------------------------------------------------
-- * Graph utilities

-- | A directed graph with nodes of type @n@ and edge labels of type
-- @l@.  Parallel edges are not allowed.
type Graph n l = M.Map n (M.Map n l)

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
