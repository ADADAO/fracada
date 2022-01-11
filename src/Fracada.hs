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

module Fracada where

import           Prelude                ( String, show, Show)
import           Control.Monad          hiding (fmap)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
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
-- import Data.Char (GeneralCategory(CurrencySymbol))

data ContractInfo = ContractInfo
    { owner :: !ValidatorHash -- ValidatorHash
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ContractInfo [('ContractInfo , 0)]
PlutusTx.makeLift ''ContractInfo

data FractionNFTDatum = FractionNFTDatum {
      tokensClass     :: AssetClass,
      totalFractions  :: Integer
    } deriving (Generic, Show)

PlutusTx.makeLift ''FractionNFTDatum
PlutusTx.makeIsDataIndexed ''FractionNFTDatum [('FractionNFTDatum,0)]

data ToFraction = ToFraction
    { nftAsset :: !AssetClass
    , fractions   :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''ToFraction
PlutusTx.makeIsDataIndexed ''ToFraction [('ToFraction,0)]

data Redeem = Redeem
    { nftAsset' :: !AssetClass
    , fractions'   :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Redeem
PlutusTx.makeIsDataIndexed ''Redeem [('Redeem,0)]

data FractionAction = To ToFraction | Re Redeem
    deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''FractionAction [ ('To,       0) -- Something isn't working here, I'm not even sure exactly what it is. TODO fix so that it compiles.
                                            , ('Re,    1)
                                            ]
PlutusTx.makeLift ''FractionAction

data ToFractionMint = ToFractionMint
    { nftSymbol     :: !BuiltinByteString
    , nftName       :: !BuiltinByteString
    , fractionQuant :: !Integer
    } deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''ToFractionMint
PlutusTx.makeIsDataIndexed ''ToFractionMint [('ToFractionMint,0)]

-- | Datum and redeemer parameter types for fractioning script
data Fractioning
instance Scripts.ValidatorTypes Fractioning where
    type instance RedeemerType Fractioning = FractionAction
    type instance DatumType Fractioning = FractionNFTDatum

{-# INLINABLE fractionNftValidator #-} -- TODO modify
fractionNftValidator :: FractionNFTDatum -> FractionAction -> ScriptContext -> Bool
fractionNftValidator FractionNFTDatum{tokensClass, totalFractions} action ctx = case action of
  To _ ->
    False
  Re redeem@Redeem{..} ->
    let
        txInfo = scriptContextTxInfo ctx

        -- make sure the asset is spent
        assetIsReturned = assetClassValueOf (valueProduced txInfo) nftAsset' > 0

        forgedTokens = assetClassValueOf (txInfoMint txInfo) tokensClass
        tokensBurnt = (forgedTokens == negate totalFractions)  && forgedTokens /= 0
    in
        traceIfFalse "NFT not returned" assetIsReturned &&
        traceIfFalse "Tokens not burn" tokensBurnt


fractionNftValidatorInstance :: Scripts.TypedValidator Fractioning
fractionNftValidatorInstance = Scripts.mkTypedValidator @Fractioning
    $$(PlutusTx.compile [|| fractionNftValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @FractionNFTDatum @FractionAction

fractionNftValidatorHash :: ValidatorHash
fractionNftValidatorHash = Scripts.validatorHash fractionNftValidatorInstance

fractionValidatorScript :: Validator
fractionValidatorScript = Scripts.validatorScript fractionNftValidatorInstance

fractionNftValidatorAddress :: Address
fractionNftValidatorAddress = Ledger.scriptAddress fractionValidatorScript

{-- {-# INLINABLE mintFractionTokens #-}
mintFractionTokens :: ContractInfo -> ValidatorHash -> FractionAction -> ScriptContext -> Bool
mintFractionTokens contractInfo@ContractInfo{..} fractionNFTScript action ctx = case action of
  To toFraction@ToFraction{..} ->
    let
      fractionTokenName = assetName nftAsset
      info = scriptContextTxInfo ctx
      mintedAmount = case flattenValue (txInfoMint info) of
          [(cs, fractionTokenName', amt)] | cs == ownCurrencySymbol ctx && fractionTokenName' == fractionTokenName -> amt
          _                                                           -> 0
    in
      (mintedAmount > 0) &&
      (fractionTokenName /= "") &&
        let
          lockedByNFTfractionScript = valueLockedBy info fractionNFTScript
          assetIsLocked = assetClassValueOf lockedByNFTfractionScript nftAsset == 1

        in
          traceIfFalse "Asset not locked" assetIsLocked           &&
          traceIfFalse "wrong fraction tokens minted" ( mintedAmount == fractions ) &&
          geq (valueLockedBy info owner) (Ada.lovelaceValueOf 2500000)
  Re redeem@Redeem{..} ->
    let
      info = scriptContextTxInfo ctx
      mintedAmount = case flattenValue (txInfoMint info) of
          [(cs, fractionTokenName', amt)] | cs == ownCurrencySymbol ctx && fractionTokenName' == assetName nftAsset' -> amt
          _                                                           -> 0
    in
      (mintedAmount < 0) &&
        let
          -- make sure the asset is spent
          assetIsReturned = assetClassValueOf (valueProduced info) nftAsset' > 0
        in
          traceIfFalse "Asset not returned" assetIsReturned           &&
          traceIfFalse "wrong fraction tokens burned" ( mintedAmount == negate fractions' )

{-# INLINABLE mintFractionTokensPara #-}
mintFractionTokensPara :: FractionAction -> ScriptContext -> Bool
mintFractionTokensPara = mintFractionTokens contractInfo fractionNftValidatorHash

mintFractionTokensPolicy :: ContractInfo -> ValidatorHash -> Scripts.MintingPolicy
mintFractionTokensPolicy contractInfo fractionNftValidatorHash = mkMintingPolicyScript
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mintFractionTokensPara ||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol (mintFractionTokensPolicy contractInfo fractionNftValidatorHash) --}