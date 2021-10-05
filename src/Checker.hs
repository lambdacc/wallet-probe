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

module Checker(endpoints, CheckerSchema) where

import           Control.Lens           (view)
import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import qualified Data.Map               as Map hiding (empty)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Issuer      as I hiding (endpoints)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Show (..), String, Semigroup (..) )
import           Wallet.Emulator.Wallet (Wallet, walletAddress, walletPubKey)

data FindParam = FindParam
    { nftTokenName :: TokenName
    , issuerWallet :: !Wallet
    , holderWallet :: !Wallet
    } deriving (Generic, ToJSON, FromJSON, ToSchema)
    
type CheckerSchema = Endpoint "findNFT" FindParam

findNFT :: forall w s e. AsContractError e => FindParam -> Contract w s e ()
findNFT param = do
    pkh <- pubKeyHash <$> ownPubKey
    let w  = holderWallet param
        iw = issuerWallet param
        nftAssetClass = AssetClass (I.issuerCS $ (pubKeyHash . walletPubKey) iw, nftTokenName param)
    os  <- map snd . Map.toList <$> utxosAt (walletAddress w)
    let nftVal = mconcat [view ciTxOutValue o | o <- os, nf (view ciTxOutValue o) nftAssetClass]
        qty = assetClassValueOf (nftVal) nftAssetClass
    logInfo @String $ "Total value at client wallet" <> (show nftVal)
    logInfo @String $ "Searching for NFT " <> (show nftAssetClass)
    logInfo @String $ "Find NFT result - " ++ (if qty == 0 then "NOT FOUND" else "FOUND")
    where
      nf val assetClass = assetClassValueOf (val) assetClass == 1

findNFT' :: Promise () CheckerSchema Text ()
findNFT' = endpoint @"findNFT" findNFT

endpoints :: AsContractError e => Contract () CheckerSchema Text e
endpoints = do
    logInfo @String "Waiting for request."
    selectList [findNFT'] >>  endpoints

mkSchemaDefinitions ''CheckerSchema
mkKnownCurrencies []
