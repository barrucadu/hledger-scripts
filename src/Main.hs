{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow      ((***))
import           Data.Function      (on)
import           Data.List          (groupBy, inits, mapAccumL, nub, sortOn)
import qualified Data.Map           as M
import           Data.Maybe         (mapMaybe)
import           Data.Semigroup     ((<>))
import           Data.String        (IsString, fromString)
import qualified Data.Text          as T
import           Data.Time.Clock    (UTCTime (..))
import           Database.InfluxDB  as I
import           Hledger.Data.Types as H
import           Hledger.Read       as H

main :: IO ()
main = do
  journal <- H.defaultJournal
  let measurements = toMeasurements (H.jtxns journal)
  I.writeBatch (I.writeParams "finance") measurements
  putStrLn $ "Wrote " ++ show (length measurements) ++ " measurements."

toMeasurements :: [H.Transaction] -> [I.Line UTCTime]
toMeasurements txns =
  measurements normalValue "normal_raw" txns
    ++ measurements normalValue "normal_total" (mapMaybe squish daily)
    ++ measurements costValue   "cost_raw"     txns
    ++ measurements costValue   "cost_total"   (mapMaybe squish daily)
 where
  daily = groupBy ((==) `on` H.tdate) txns
  squish ts@(t:_) = Just t { H.tdescription = "aggregate"
                           , H.tpostings    = concatMap H.tpostings ts
                           }
  squish _ = Nothing

  measurements value name =
    concat
      . snd
      . mapAccumL
          ( toInflux value
                     (fromText (name <> "_raw"))
                     (fromText (name <> "_delta"))
          )
          M.empty

toInflux
  :: (H.MixedAmount -> [(T.Text, Double)])
  -> I.Measurement
  -> I.Measurement
  -> M.Map I.Key Double
  -> H.Transaction
  -> (M.Map I.Key Double, [I.Line UTCTime])
toInflux value keyT keyD bals txn =
  (bals', map toLine [(keyT, fieldsT), (keyD, fieldsD)])
 where
  toLine (k, fs) = Line k tags fs (Just time)
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
  fieldsT = fmap I.FieldFloat bals'
  fieldsD = fmap I.FieldFloat deltas
  bals'   = M.unionWith (+) bals deltas
  deltas  = toDeltas value txn

toDeltas
  :: (H.MixedAmount -> [(T.Text, Double)])
  -> H.Transaction
  -> M.Map I.Key Double
toDeltas value txn =
  let postings = concatMap explodeAccount (H.tpostings txn)
      accounts = nub (map H.paccount postings)
  in  M.fromList
        [ (fromString (T.unpack (a <> "[" <> cur <> "]")), val)
        | a <- accounts
        , let ps = filter ((== a) . H.paccount) postings
        , (cur, val) <- sumSame (concatMap (value . H.pamount) ps)
        ]

-------------------------------------------------------------------------------

explodeAccount :: H.Posting -> [H.Posting]
explodeAccount p =
  [ p { H.paccount = a }
  | a <- tail . map (T.intercalate ":") . inits . T.splitOn ":" $ H.paccount p
  ]

normalValue :: H.MixedAmount -> [(T.Text, Double)]
normalValue (H.Mixed amounts) = map go amounts
  where go (H.Amount c q _ _ _) = (c, fromRational (toRational q))

costValue :: H.MixedAmount -> [(T.Text, Double)]
costValue (H.Mixed amounts) = map go amounts
 where
  go (H.Amount _ _ (H.TotalPrice a) _ _) = go a
  go (H.Amount c q _                _ _) = (c, fromRational (toRational q))

sumSame :: (Ord k, Num v) => [(k, v)] -> [(k, v)]
sumSame = go . sortOn fst
 where
  go ((k1, v1):(k2, v2):rest) | k1 == k2  = go ((k1, v1 + v2) : rest)
                              | otherwise = (k1, v1) : go ((k2, v2) : rest)
  go xs = xs

fromText :: IsString s => T.Text -> s
fromText = fromString . T.unpack

showStatus :: H.Status -> T.Text
showStatus H.Cleared  = "cleared"
showStatus H.Pending  = "pending"
showStatus H.Unmarked = "uncleared"
