{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Issuer(issuerCS, endpoints, IssuerSchema) where

import           Control.Lens           (view)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import qualified Data.Map               as Map hiding (empty)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Builtins.Class
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..) )
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet (Wallet, WalletId, walletPubKey, getWalletId)

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode pkh

issuerCS :: PubKeyHash -> CurrencySymbol
issuerCS = scriptCurrencySymbol . policy

data IssueParam = IssueParam
    { tn      :: String
    , destW   :: Wallet
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
    
type IssuerSchema = Endpoint "mint" IssueParam
                .\/ Endpoint "logCS" ()

mint :: forall w s e. AsContractError e => IssueParam -> Contract w s e ()
mint param = do
    pkh <- pubKeyHash <$> ownPubKey
    utxos <- utxosAt $ pubKeyHashAddress pkh
    let destWallet = destW param
        destPkh = (pubKeyHash . walletPubKey) $ destWallet
        tName   = tn param
        val     = Value.singleton (issuerCS pkh) (TokenName $ stringToBuiltinByteString tName) 1
        lookups = Constraints.mintingPolicy (policy pkh) <>
                  Constraints.unspentOutputs utxos
        tx      = Constraints.mustMintValue val <>
                  Constraints.mustPayToPubKey destPkh val
    logInfo @String $ printf "Minting NFT %s" (show val)
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Issued NFT %s" (show val)

logCS :: forall w s e. AsContractError e => () -> Contract w s e ()
logCS () = do
    pkh <- pubKeyHash <$> ownPubKey
    let cs = issuerCS pkh
    logInfo @String
            $ "Logging own nft token name : " <> show cs
    logInfo @String $ "logCS complete"

mint' :: Promise () IssuerSchema Text ()
mint' = endpoint @"mint" mint

logCS' :: Promise () IssuerSchema Text ()
logCS' = endpoint @"logCS" logCS

endpoints :: AsContractError e => Contract () IssuerSchema Text e
endpoints = do
    logInfo @String "Waiting for request."
    selectList [mint',logCS'] >>  endpoints

mkSchemaDefinitions ''IssuerSchema
mkKnownCurrencies []
