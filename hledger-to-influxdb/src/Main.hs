{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow      ((***))
import           Data.Decimal       (Decimal)
import           Data.Foldable      (for_)
import           Data.Function      (on)
import           Data.List          (groupBy, mapAccumL, nub)
import qualified Data.Map           as M
import           Data.Maybe         (mapMaybe)
import           Data.Semigroup     ((<>))
import           Data.String        (IsString, fromString)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import           Data.Time.Clock    (UTCTime (..))
import           Database.InfluxDB  as I
import           Hledger.Data.Types as H
import           Hledger.Read       as H

import qualified Extras             as E

main :: IO ()
main = do
  journal <- H.defaultJournal
  let measurements = toMeasurements (H.jmarketprices journal) (H.jtxns journal)
  for_ measurements $ \(name, ms) -> do
    putStrLn ("Writing " <> T.unpack name <> " (" <> show (length ms) <> " measurements)")
    for_ (chunksOf 300 ms) (I.writeBatch (I.writeParams "finance"))
  putStrLn "Done"

toMeasurements
  :: [H.MarketPrice] -> [H.Transaction] -> [(T.Text, [I.Line UTCTime])]
toMeasurements prices txns =
  [ measurements nvalue        "normal_txns"    txns
  , measurements nvalue        "normal_dailies" dailies
  , measurements cvalue        "cost_txns"      txns
  , measurements cvalue        "cost_dailies"   dailies
  , measurements mvalue        "market_txns"    txns
  , measurements mvalue        "market_dailies" dailies
  , measurements countToInflux "count"          txns
  , measurements priceToInflux "market"         prices
  ]
 where
  nvalue = balancesToInflux (const E.normalValue)
  cvalue = balancesToInflux (const E.costValue)
  mvalue = toInflux accountKey
                    (E.marketValue (E.buildPrices prices))
                    (toDeltas (const E.normalValue))

  dailies = mapMaybe squish $ groupBy ((==) `on` H.tdate) txns
  squish ts@(t:_) = Just t { H.tdescription = "aggregate"
                           , H.tpostings    = concatMap H.tpostings ts
                           }
  squish _ = Nothing

  measurements toL name xs =
    let go start = mapAccumL
          (toL (fromText (name <> "_total")) (fromText (name <> "_delta")))
          start
          xs
        initialAccounts = M.map (const 0) (fst (go M.empty))
    in  (name, concat (snd (go initialAccounts)))

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
      . map (toKey *** (I.FieldFloat . E.doub))
      . E.sumSame
      . concatMap (uncurry (transform (H.tdate txn)))
      . M.toList

priceToInflux
  :: a
  -> b
  -> M.Map (H.CommoditySymbol, H.CommoditySymbol) Decimal
  -> H.MarketPrice
  -> ( M.Map (H.CommoditySymbol, H.CommoditySymbol) Decimal
     , [I.Line UTCTime]
     )
priceToInflux _ _ state (H.MarketPrice day c (H.Amount c' q' _ _ _)) =
  (state', [Line "market" M.empty fields (Just (UTCTime day 0))])
 where
  fields   = toFields state'
  state'   = M.insert (c, c') q' state
  toFields = M.fromList . map (priceKey *** (I.FieldFloat . E.doub)) . M.toList

toDeltas
  :: (Day -> H.MixedAmount -> [(H.CommoditySymbol, Decimal)])
  -> H.Transaction
  -> [((H.AccountName, H.CommoditySymbol), Decimal)]
toDeltas value txn =
  let postings = concatMap E.explodeAccount (H.tpostings txn)
      accounts = nub (map H.paccount postings)
  in  [ ((a, cur), val)
      | a <- accounts
      , let ps = filter ((== a) . H.paccount) postings
      , (cur, val) <- E.sumSame (concatMap (value (tdate txn) . H.pamount) ps)
      ]

-------------------------------------------------------------------------------

fromText :: IsString s => T.Text -> s
fromText = fromString . T.unpack

showStatus :: H.Status -> T.Text
showStatus H.Cleared  = "cleared"
showStatus H.Pending  = "pending"
showStatus H.Unmarked = "uncleared"

accountKey :: (H.AccountName, H.CommoditySymbol) -> I.Key
accountKey (a, c) = fromText $ a <> "[" <> c <> "]"

priceKey :: (H.CommoditySymbol, H.CommoditySymbol) -> I.Key
priceKey (c, c') = fromText $ c <> "[" <> c' <> "]"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
