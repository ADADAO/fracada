
{-# LANGUAGE OverloadedStrings   #-}
import           Prelude
import           System.Environment

import           Cardano.Api
import           Cardano.Api.Shelley
import           Codec.Serialise

import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Plutus.V1.Ledger.Api as Plutus

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB

import           Ledger                     (datumHash)


import           Fracada
import           FractionToken

import          Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Api
import Data.String                         (IsString (..))
import           Data.Aeson

-- test data
-- nftCurrencySymbol = fromString  "6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7" 
-- nftTokenName =  "" 
-- fractionTokenName =  "FracadaToken" 
-- numberOfFractions = 10 :: Integer 
-- nft = AssetClass (nftCurrencySymbol, nftTokenName)
-- fractionToken = Plutus.TokenName fractionTokenName

contractInfo = ContractInfo
    { owner = ("75e003e0749ce453fc419933297de37f4e859324dc79d1156b3699f9")
    }

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  if nargs /= 0 then
    do
      putStrLn $ "Usage:"
      putStrLn $ "script-dump"
  else 
    do 
      let       
        validatorname = "validator.plutus"
        mintingname = "minting.plutus"

        validatorAsCbor = serialise fractionValidatorScript
        validatorShortBs = SBS.toShort . LB.toStrict $ validatorAsCbor
        validatorScript = PlutusScriptSerialised validatorShortBs

        appliedMintingPolicy = mintFractionTokensPolicy contractInfo fractionNftValidatorHash

        mintingAsValidator = Plutus.Validator $ Plutus.unMintingPolicyScript appliedMintingPolicy
        mintingAsCbor = serialise mintingAsValidator
        mintingScriptShortBs = SBS.toShort . LB.toStrict $ mintingAsCbor
        mintingScript = PlutusScriptSerialised mintingScriptShortBs

      putStrLn $ "Writing output to: " ++ validatorname
      writePlutusScript validatorname validatorScript validatorShortBs

      writeFile "validator-hash.txt" (show $ fractionNftValidatorHash)

      putStrLn $ "Writing output to: " ++ mintingname
      writePlutusScript mintingname mintingScript mintingScriptShortBs      

      writeFile "currency-id.txt" (show $ curSymbol contractInfo fractionNftValidatorHash)        

writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript filename scriptSerial scriptSBS =
  do
  case Plutus.defaultCostModelParams of
        Just m -> print "Working."
          {-- let Alonzo.Data pData = toAlonzoData (ScriptDataNumber scriptnum)
              -- (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget --}
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
