module Extras where

import           Control.Arrow      (second)
import           Data.Decimal       (Decimal)
import           Data.List          (inits, sortOn)
import qualified Data.Map           as M
import           Data.Semigroup     ((<>))
import           Data.Time.Calendar (Day)
import           Hledger.Data.Types as H

import qualified Graph              as G

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
  go (H.Amount _ q (H.UnitPrice  a) _ _) = second (* q) (go a)
  go (H.Amount _ q (H.TotalPrice a) _ _) = second (* signum q) (go a)
  go (H.Amount c q H.NoPrice        _ _) = (c, q)

-- | The \"market\" value of an amount, as of a given day.
--
-- Unlike 'normalValue' and 'costValue', this takes a single commodity
-- rather than a 'H.MixedAmount'.
marketValue
  :: [(Day, G.Graph H.CommoditySymbol Decimal)]
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
buildPrices :: [H.MarketPrice] -> [(Day, G.Graph H.CommoditySymbol Decimal)]
buildPrices prices0 =
  reverse . map (second close) . graphs G.empty . sortOn fst $ prices
 where
  prices = [ (H.mpdate p, (H.mpcommodity p, H.mpamount p)) | p <- prices0 ]

  graphs g ((day, (c, H.Amount c' q _ _ _)):rest) =
    let g' = G.addEdge const c c' q . G.addNode c' . G.addNode c $ g
    in  case rest of
          ((day', _):_) | day == day' -> graphs g' rest
          _                           -> (day, g') : graphs g' rest
  graphs _ [] = []

  close =
    G.transitiveClosure (*) . G.symmetricClosure (1 /) . G.reflexiveClosure 1

-- | Find all the exchange rates for a commodity.
findFactors
  :: H.CommoditySymbol
  -> G.Graph H.CommoditySymbol Decimal
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
