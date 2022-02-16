{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module NFT.NFTweetMint
  ( MintParams (..)
  , nftMint
  , mintingScriptShortBs
  ) where

import           Control.Monad          hiding (fmap)
-- import qualified Data.Map               as Map
-- import           Data.Text              (Text)
-- import           Data.Void              (Void)
-- import           Plutus.Contract        as Contract
-- import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
-- import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
-- import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
-- import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
-- import           Wallet.Emulator.Wallet
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import qualified Plutus.V1.Ledger.Scripts as Plutus
import Plutus.V1.Ledger.Contexts (TxInfo)
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short    as SBS
import           Codec.Serialise

data MintParams = MintParams
    { mpTokenName :: !TokenName
    , mpAmount    :: !Integer
    }

PlutusTx.makeLift ''MintParams

{-# INLINABLE mkNFTweetPolicy #-}
mkNFTweetPolicy :: MintParams -> TxOutRef -> BuiltinData -> ScriptContext -> Bool
mkNFTweetPolicy mp oref _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                    traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(cs, tn, amt)] -> cs  == ownCurrencySymbol ctx && tn == (mpTokenName mp) && amt == (mpAmount mp)
        _                -> False

nftPolicy :: MintParams -> TxOutRef -> Scripts.MintingPolicy
nftPolicy mp oref = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \mp' oref' -> Scripts.wrapMintingPolicy $ mkNFTweetPolicy mp' oref' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode mp
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref

-- For creating script
nftPlutusScript :: MintParams -> TxOutRef -> Script
nftPlutusScript mp = unMintingPolicyScript . nftPolicy mp

nftValidator :: MintParams -> TxOutRef -> Validator
nftValidator mp = Validator . nftPlutusScript mp

nftScriptAsCbor :: MintParams -> TxOutRef -> LB.ByteString
nftScriptAsCbor mp = serialise . nftValidator mp

nftMint :: MintParams -> TxOutRef -> PlutusScript PlutusScriptV1
nftMint mp
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . nftScriptAsCbor mp

mintingScriptShortBs :: MintParams -> TxOutRef -> SBS.ShortByteString
mintingScriptShortBs mp tx = SBS.toShort . LB.toStrict $ nftScriptAsCbor mp tx

-- For emulating:

-- curSymbol :: TxOutRef -> MintParams -> CurrencySymbol
-- curSymbol oref mp = scriptCurrencySymbol $ policy oref mp

-- type NFTSchema = Endpoint "mint" MintParams

-- mint :: MintParams -> Contract w NFTSchema Text ()
-- mint mp = do
--     pk    <- Contract.ownPubKey
--     utxos <- utxoAt (pubKeyAddress pk)
--     case Map.keys utxos of
--         []       -> Contract.logError @String "no utxo found"
--         oref : _ -> do
--             let val     = Value.singleton (curSymbol oref tn) tn 1
--                 lookups = Constraints.mintingPolicy (policy oref mp) <> Constraints.unspentOutputs utxos
--                 tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
--             ledgerTx <- submitTxConstraintsWith @Void lookups tx
--             void $ awaitTxConfirmed $ txId ledgerTx
--             Contract.logInfo @String $ printf "forged NFT: %s" (show val)

-- endpoints :: Contract () NFTSchema Text ()
-- endpoints = mint' >> endpoints
--   where
--     mint' = endpoint @"mint" >>= mint

-- mkSchemaDefinitions ''NFTSchema

-- mkKnownCurrencies []

-- test :: IO ()
-- test = runEmulatorTraceIO $ do
--     let tn = "ABC"
--     h1 <- activateContractWallet (Wallet 1) endpoints
--     h2 <- activateContractWallet (Wallet 2) endpoints
--     callEndpoint @"mint" h1 $ MintParams
--         { mpTokenName = tn
--         , mpAmount    = 555
--         }
--     callEndpoint @"mint" h2 $ MintParams
--         { mpTokenName = tn
--         , mpAmount    = 444
--         }
--     void $ Emulator.waitNSlots 1
--     callEndpoint @"mint" h1 $ MintParams
--         { mpTokenName = tn
--         , mpAmount    = -222
--         }
--     void $ Emulator.waitNSlots 1