{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus
import  Plutus.V1.Ledger.Value
import           NFT.NFTweetMint
import qualified PlutusTx
import Plutus.V1.Ledger.Scripts
import Data.String
import qualified Data.ByteString.Lazy.UTF8 (fromString)
import Data.Int
import Text.Read
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson (ToJSON)

isInt64Bounded :: Integer -> Bool
isInt64Bounded amt = 
    if ((amt >= (fromIntegral (minBound :: Int64)::Integer)) && (amt <= (fromIntegral (maxBound :: Int64)::Integer))) then 
        True
    else False

isValidAmt :: String -> Bool
isValidAmt amt =
    let maybeAmt = readMaybe amt :: Maybe Integer in
    case maybeAmt of
        Just intAmt -> 
            if (isInt64Bounded intAmt) then
                True
            else False
        Nothing -> False

main :: IO ()
main = do
    [tn, amt, utxo', fileName, evalJsonFile] <- getArgs
    let utxo            = parseUTxO utxo'
        nftPolicyFile   = fileName
        isValidAmount   = isValidAmt amt
    if (isValidAmount) then do
      case Plutus.defaultCostModelParams of
        Just m -> let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m (mintingScriptShortBs (MintParams { mpTokenName = (tokenName $ fromString tn),
                                mpAmount = (read amt :: Integer)}) utxo) []
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> do 
                        print ("Ex Budget" :: String) >> print exbudget
                        I.writeFile evalJsonFile (encodeToLazyText exbudget)
        Nothing -> error "defaultCostModelParams failed"
      nftPolicyResult <- writeFileTextEnvelope nftPolicyFile Nothing $ 
          nftMint (MintParams { mpTokenName = (tokenName $ fromString tn),
                                mpAmount = (read amt :: Integer)})
                  utxo
      case nftPolicyResult of
        Left err -> print $ displayError err
        Right () -> Prelude.putStrLn $ "wrote NFT policy to file " ++ nftPolicyFile
    else do
      Prelude.putStrLn $ "not a valid amount " ++ amt
      return()

parseUTxO :: String -> Plutus.TxOutRef
parseUTxO s =
  let
    (x, y) = span (/= '#') s
  in
    Plutus.TxOutRef (Plutus.TxId $ Plutus.getLedgerBytes $ fromString x) $ read $ tail y
