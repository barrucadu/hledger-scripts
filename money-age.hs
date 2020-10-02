#!/usr/bin/env stack
{- stack script
  --nix --no-nix-pure
  --resolver lts-16.16
  --package boxes,containers,Decimal,hledger-lib,text,time
-}

{-# OPTIONS_GHC -Weverything -Wno-implicit-prelude -Wno-missing-export-lists -Wno-unsafe #-}

{-# LANGUAGE OverloadedStrings #-}

import           Data.Decimal             (Decimal)
import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe, maybeToList)
import qualified Data.Text                as T
import           Data.Time.Calendar       (Day, diffDays)
import           Data.Time.Clock          (UTCTime(utctDay), getCurrentTime)
import qualified Hledger.Data.Types       as H
import qualified Hledger.Read             as H
import qualified Text.PrettyPrint.Boxes   as B
import           Text.Printf              (printf)

main :: IO ()
main = do
  journal <- H.defaultJournal
  today   <- utctDay <$> getCurrentTime
  printAccountStats (calcAccountStats journal today)

-------------------------------------------------------------------------------

data AccountStats = AccountStats
  { aAgeOfOldestGBP :: Integer
  , aAgeOfAverageGBP :: Decimal
  , aAverageLifespanOfSpentGBP :: Maybe Decimal
  }

calcAccountStats :: H.Journal -> Day -> M.Map H.AccountName AccountStats
calcAccountStats journal today =
  M.mapMaybe (doAccountStats today) $
  foldl doTransaction M.empty (H.jtxns journal)

printAccountStats :: M.Map H.AccountName AccountStats -> IO ()
printAccountStats stats = B.printBox $ B.hsep 1 B.top
    [ col B.left "account" T.unpack (M.keys stats)
    , col B.right "age of oldest £" (show . aAgeOfOldestGBP) vals
    , col B.right "age of average £" (roundDecimal . aAgeOfAverageGBP) vals
    , col B.right "average livespan of spent £" (maybe "-" roundDecimal . aAverageLifespanOfSpentGBP) vals
    ]
  where
    vals = M.elems stats

    col :: B.Alignment -> String -> (a -> String) -> [a] -> B.Box
    col a hdr f bs = B.vcat a (map B.text (hdr:"---":map f bs))

-------------------------------------------------------------------------------

type Pots = ([(Integer, Decimal)], M.Map Day Decimal)

doTransaction :: M.Map H.AccountName Pots -> H.Transaction -> M.Map H.AccountName Pots
doTransaction ages0 txn = foldl (doPosting day) ages0 expandedPostings where
  day = H.tdate txn
  expandedPostings =
    [ (H.paccount posting, amount)
    | posting <- filter (check . H.paccount) (H.tpostings txn)
    , amount <- maybeToList (toGBP (H.pamount posting))
    ]

  check account =
    ("assets:cash" `T.isPrefixOf` account) &&
    not ("assets:cash:petty" `T.isPrefixOf` account) &&
    not ("assets:cash:nationwide:flexdirect:pending" `T.isPrefixOf` account)

doPosting :: Day -> M.Map H.AccountName Pots -> (H.AccountName, Decimal) -> M.Map H.AccountName Pots
doPosting day ages (account, amount)
    | amount > 0 = M.alter (Just . addMoneyToPot)   account ages
    | amount < 0 = M.alter (Just . delMoneyFromPot) account ages
    | otherwise = ages
  where
    addMoneyToPot :: Maybe Pots -> Pots
    addMoneyToPot (Just (ls, pots)) = (ls, M.alter (\a -> Just (amount + fromMaybe 0 a)) day pots)
    addMoneyToPot Nothing = ([], M.fromList [(day, amount)])

    delMoneyFromPot :: Maybe Pots -> Pots
    delMoneyFromPot (Just (ls, pots)) = delMoneyFromPot' (abs amount) ls (M.toList pots)
    delMoneyFromPot Nothing = debt amount

    delMoneyFromPot' :: Decimal -> [(Integer, Decimal)] -> [(Day, Decimal)] -> Pots
    delMoneyFromPot' q ls ((d,p):ps)
      | q > p = delMoneyFromPot' (q - p) ((diffDays day d, p):ls) ps
      | q < p = ((diffDays day d, p - q):ls, M.fromList ((d,p - q):ps))
      | otherwise = ((diffDays day d, p):ls, M.fromList ps)
    delMoneyFromPot' q _ [] = debt q

    debt :: Decimal -> a
    debt q = error ("[" ++ T.unpack account ++ ", " ++ show day ++ "] tried to go into debt by " ++ show (abs q))

doAccountStats :: Day -> Pots -> Maybe AccountStats
doAccountStats today (ls, pots)
  | M.null pots = Nothing
  | otherwise = Just $ AccountStats
    { aAgeOfOldestGBP = diffDays today (fst (head ds))
    , aAgeOfAverageGBP = weightedAvg [(fromIntegral (diffDays today d), w) | (d, w) <- ds]
    , aAverageLifespanOfSpentGBP =
      let ls' = filter ((/=0) . fst) ls
      in if null ls'
         then Nothing
         else Just (weightedAvg [(fromInteger d, w) | (d, w) <- ls'])
    }
  where
    ds = M.toList pots

-------------------------------------------------------------------------------

weightedAvg :: Fractional a => [(a,a)] -> a
weightedAvg xws = sum [x * w | (x, w) <- xws] / sum [w | (_, w) <- xws]

roundDecimal :: Decimal -> String
roundDecimal = printf "%0.3f" . (realToFrac :: Decimal -> Double)

toGBP :: H.MixedAmount -> Maybe Decimal
toGBP (H.Mixed [H.Amount "£" q _ _ _]) = Just q
toGBP _ = Nothing
