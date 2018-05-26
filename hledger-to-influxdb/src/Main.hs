{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow      (second, (***))
import           Data.Decimal       (Decimal)
import           Data.Function      (on)
import           Data.List          (groupBy, inits, mapAccumL, nub, sortBy,
                                     sortOn)
import qualified Data.Map           as M
import           Data.Maybe         (mapMaybe)
import           Data.Semigroup     ((<>))
import qualified Data.Set           as S
import           Data.String        (IsString, fromString)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import           Data.Time.Clock    (UTCTime (..))
import           Database.InfluxDB  as I
import           Hledger.Data.Types as H
import           Hledger.Read       as H

main :: IO ()
main = do
  journal <- H.defaultJournal
  let measurements = toMeasurements (H.jmarketprices journal) (H.jtxns journal)
  I.writeBatch (I.writeParams "finance") measurements
  putStrLn $ "Wrote " ++ show (length measurements) ++ " measurements."

toMeasurements :: [H.MarketPrice] -> [H.Transaction] -> [I.Line UTCTime]
toMeasurements prices txns =
  measurements nvalue "normal_txns" txns
    ++ measurements nvalue        "normal_dailies" dailies
    ++ measurements cvalue        "cost_txns"      txns
    ++ measurements cvalue        "cost_dailies"   dailies
    ++ measurements mvalue        "market_txns"    txns
    ++ measurements mvalue        "market_dailies" dailies
    ++ measurements countToInflux "count"          txns
 where
  nvalue = balancesToInflux normalValue
  cvalue = balancesToInflux costValue
  mvalue = toInflux accountKey
                    (marketValue (buildPrices prices))
                    (toDeltas normalValue)

  dailies = mapMaybe squish $ groupBy ((==) `on` H.tdate) txns
  squish ts@(t:_) = Just t { H.tdescription = "aggregate"
                           , H.tpostings    = concatMap H.tpostings ts
                           }
  squish _ = Nothing

  measurements toL name =
    concat
      . snd
      . mapAccumL
          (toL (fromText (name <> "_total")) (fromText (name <> "_delta")))
          M.empty

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
  -> I.Measurement
  -> M.Map k Decimal
  -> H.Transaction
  -> (M.Map k Decimal, [I.Line UTCTime])
toInflux toKey transform deltaf keyT keyD state txn =
  (state', map toLine [(keyT, fieldsT), (keyD, fieldsD)])
 where
  toLine (key, fields) = Line key tags fields (Just time)
  time = UTCTime (H.tdate txn) 0
  tags =
    M.fromList
      .  map (fromText *** fromText)
      .  filter (not . T.null . snd)
      $  [ ("code"       , H.tcode txn)
         , ("description", H.tdescription txn)
         , ("status"     , showStatus (H.tstatus txn))
         ]
      ++ H.ttags txn

  fieldsT = toFields state'
  fieldsD = toFields deltas

  state'  = M.unionWith (+) state deltas
  deltas  = M.fromList (deltaf txn)

  toFields =
    M.fromList
      . map (toKey *** (I.FieldFloat . doub))
      . sumSame
      . concatMap (uncurry (transform (H.tdate txn)))
      . M.toList

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

-------------------------------------------------------------------------------

normalValue :: a -> H.MixedAmount -> [(H.CommoditySymbol, Decimal)]
normalValue _ (H.Mixed amounts) = map go amounts
  where go (H.Amount c q _ _ _) = (c, q)

costValue :: a -> H.MixedAmount -> [(H.CommoditySymbol, Decimal)]
costValue _ (H.Mixed amounts) = map go amounts
 where
  go (H.Amount _ q (H.TotalPrice a) _ _) = second (* signum q) (go a)
  go (H.Amount c q _                _ _) = (c, q)

marketValue
  :: M.Map H.CommoditySymbol [(Day, H.Amount)]
  -> Day
  -> (H.AccountName, H.CommoditySymbol)
  -> Decimal
  -> [((H.AccountName, H.CommoditySymbol), Decimal)]
marketValue prices day (a, c) q =
  [ ((a, c'), factor * q) | (c', factor) <- (c, 1) : factors ]
  where factors = findFactors day (M.findWithDefault [] c prices)

-------------------------------------------------------------------------------

buildPrices :: [H.MarketPrice] -> M.Map H.CommoditySymbol [(Day, H.Amount)]
buildPrices = fmap (sortBy (flip compare)) . foldr go M.empty
  where go p = M.insertWith (++) (H.mpcommodity p) [(H.mpdate p, H.mpamount p)]

findFactors :: Day -> [(Day, H.Amount)] -> [(H.CommoditySymbol, Decimal)]
findFactors day = go S.empty . dropWhile ((> day) . fst)
 where
  go commodities ((_, H.Amount c q _ _ _):rest)
    | c `S.member` commodities = go commodities rest
    | otherwise                = (c, q) : go (c `S.insert` commodities) rest
  go _ [] = []

-------------------------------------------------------------------------------

explodeAccount :: H.Posting -> [H.Posting]
explodeAccount p =
  [ p { H.paccount = a }
  | a <- tail . map (T.intercalate ":") . inits . T.splitOn ":" $ H.paccount p
  ]

sumSame :: (Ord k, Num v) => [(k, v)] -> [(k, v)]
sumSame = go . sortOn fst
 where
  go ((k1, v1):(k2, v2):rest) | k1 == k2  = go ((k1, v1 + v2) : rest)
                              | otherwise = (k1, v1) : go ((k2, v2) : rest)
  go xs = xs

doub :: Decimal -> Double
doub = fromRational . toRational

fromText :: IsString s => T.Text -> s
fromText = fromString . T.unpack

showStatus :: H.Status -> T.Text
showStatus H.Cleared  = "cleared"
showStatus H.Pending  = "pending"
showStatus H.Unmarked = "uncleared"

accountKey :: (H.AccountName, H.CommoditySymbol) -> I.Key
accountKey (a, c) = fromText $ a <> "[" <> c <> "]"
