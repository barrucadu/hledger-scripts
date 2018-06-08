module Main where

import           Control.Monad            (foldM_)
import           Data.Decimal             (Decimal)
import           Data.List                (nub)
import qualified Data.Map                 as M
import           Data.Text                (unpack)
import           Hledger.Data.Transaction as H
import           Hledger.Data.Types       as H
import           Hledger.Read             as H

import qualified Extras                   as E

main :: IO ()
main = do
  journal <- H.defaultJournal
  foldM_ checkTransaction M.empty (H.jtxns journal)

checkTransaction
  :: M.Map H.AccountName Decimal
  -> H.Transaction
  -> IO (M.Map H.AccountName Decimal)
checkTransaction old txn = do
  let balances = M.unionWith (+) old (toDeltas txn)
  case check old balances of
    [] -> pure ()
    es -> do
      mapM_ putStrLn es
      putStrLn (H.showTransaction txn)
  pure balances

toDeltas :: H.Transaction -> M.Map H.AccountName Decimal
toDeltas txn =
  M.fromList
    $ let postings = concatMap E.explodeAccount (H.tpostings txn)
          accounts = nub (map H.paccount postings)
      in  [ (a, val)
          | a <- accounts
          , let ps = filter ((== a) . H.paccount) postings
          , (cur, val) <- E.sumSame (concatMap (E.costValue . H.pamount) ps)
          , unpack cur == "£"
          ]

check :: M.Map AccountName Decimal -> M.Map H.AccountName Decimal -> [String]
check previous = M.foldrWithKey' check' []
 where
  check' account value errs
    | E.isAsset account && value < 0 = case M.lookup account previous of
      Just old | old < 0 -> errs -- no error if it was previously below zero
      _ ->
        (  "Account "
          ++ unpack account
          ++ " has value "
          ++ show (E.doub value)
          ++ " which is below 0."
          )
          : errs
    | otherwise = errs
