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
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}

module FractionToken where

import           Prelude                ( String, show, Show)
import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.Builtins      as Builtins
import           PlutusTx.IsData
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Ada             as Ada hiding (divide)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import qualified Ledger.Contexts                   as Validation
import           Ledger.Value           as Value
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema, NonEmpty(..) )
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions, ensureKnownCurrencies)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..))
import           Text.Printf            (printf)
import           GHC.Generics         (Generic)
import           Data.Aeson           (ToJSON, FromJSON)
import           Playground.Contract
import           Fracada
import Data.Bool (Bool)

{-# INLINABLE theGreatFilter #-}
theGreatFilter :: BuiltinByteString -> BuiltinByteString -> CurrencySymbol -> TokenName -> Bool
theGreatFilter cb tb c t = t == (tokenName $ fromBuiltin (appendByteString cb tb))

{-# INLINABLE mintFractionTokens #-}
mintFractionTokens :: ContractInfo -> ValidatorHash -> ToFractionMint -> ScriptContext -> Bool
mintFractionTokens contractInfo@ContractInfo{..} fractionNFTScript a ctx =
    let
      info = scriptContextTxInfo ctx
      mintedAmount = case flattenValue (txInfoMint info) of
          [(cs, fractionTokenName, amt)] | cs == ownCurrencySymbol ctx && theGreatFilter (nftSymbol a) (nftName a) cs fractionTokenName -> amt
          _                                                           -> 0
      -- tokenSymbol = currencySymbol (nftSymbol a)
      -- tokenName' = tokenName (nftName a)
      nftAsset = assetClass (currencySymbol $ fromBuiltin (nftSymbol a)) (tokenName $ fromBuiltin (nftName a))
      -- AssetClass{tokenSymbol, tokenName'}
    in
      (mintedAmount > 0) &&
        let
          lockedByNFTfractionScript = valueLockedBy info fractionNFTScript
          assetIsLocked = assetClassValueOf lockedByNFTfractionScript nftAsset == 1

        in
          traceIfFalse "Asset not locked" assetIsLocked           &&
          traceIfFalse "wrong fraction tokens minted" ( mintedAmount == (fractionQuant a) ) &&
          geq (valueLockedBy info owner) (Ada.lovelaceValueOf 2500000)
      ||
      (mintedAmount < 0) &&
        let
          assetIsReturned = assetClassValueOf (valueProduced info) nftAsset > 0
        in
          traceIfFalse "Asset not returned" assetIsReturned           &&
          traceIfFalse "wrong fraction tokens burned" ( mintedAmount == negate (fractionQuant a) )

mintFractionTokensPolicy :: ContractInfo -> ValidatorHash -> Scripts.MintingPolicy
mintFractionTokensPolicy contractInfo fractionNftValidatorHash = mkMintingPolicyScript
    ($$(PlutusTx.compile [|| \contractInfo' validator' -> Scripts.wrapMintingPolicy $ mintFractionTokens contractInfo' validator' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode contractInfo
    `PlutusTx.applyCode`
    PlutusTx.liftCode fractionNftValidatorHash)

curSymbol :: ContractInfo -> ValidatorHash -> CurrencySymbol
curSymbol contractInfo validator = scriptCurrencySymbol (mintFractionTokensPolicy contractInfo validator)