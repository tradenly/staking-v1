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

module StakingContract where

import           PlutusTx
import           PlutusTx.Prelude
import           Ledger
import           Ledger.Ada
import           Ledger.Value
import           Ledger.Typed.Scripts
import           Ledger.TimeSlot
import           Plutus.Contract
import           Plutus.V1.Ledger.Api
import           Plutus.V1.Ledger.Contexts
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Interval
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Prelude (IO, Show)
import qualified Prelude

-- Define the staking parameters
data StakingParams = StakingParams
    { stakeToken :: AssetClass
    , rewardToken :: AssetClass
    , lockPeriod :: POSIXTime
    , apr :: Rational
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Define the staking actions
data StakingAction
    = Stake
    | Withdraw
    deriving (Show, Generic, ToJSON, FromJSON)

-- Define the staking datum
data StakingDatum = StakingDatum
    { staker :: PubKeyHash
    , stakedAmount :: Integer
    , stakedTime :: POSIXTime
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Define the staking redeemer
data StakingRedeemer = StakingRedeemer
    { action :: StakingAction
    } deriving (Show, Generic, ToJSON, FromJSON)

-- Define the staking validator
stakingValidator :: StakingParams -> StakingDatum -> StakingRedeemer -> ScriptContext -> Bool
stakingValidator params datum redeemer ctx =
    case action redeemer of
        Stake ->
            -- Check that the staking period is valid
            traceIfFalse "Invalid lock period" (lockPeriodValid (lockPeriod params) (stakedTime datum) (txInfoValidRange info))
            -- Check that the staked amount is correct
            && traceIfFalse "Incorrect staked amount" (stakedAmountValid (stakedAmount datum) (valuePaidTo info (staker datum)) (stakeToken params))
        Withdraw ->
            -- Check that the staking period has ended
            traceIfFalse "Staking period not ended" (lockPeriodEnded (lockPeriod params) (stakedTime datum) (txInfoValidRange info))
            -- Check that the reward is correct
            && traceIfFalse "Incorrect reward amount" (rewardAmountValid (stakedAmount datum) (lockPeriod params) (apr params) (valueProduced info) (rewardToken params))
  where
    info = scriptContextTxInfo ctx

-- Helper functions
lockPeriodValid :: POSIXTime -> POSIXTime -> Interval POSIXTime -> Bool
lockPeriodValid lockPeriod stakedTime validRange =
    intervalContains (interval stakedTime (stakedTime + lockPeriod)) validRange

stakedAmountValid :: Integer -> Value -> AssetClass -> Bool
stakedAmountValid stakedAmount value assetClass =
    assetClassValueOf value assetClass == stakedAmount

lockPeriodEnded :: POSIXTime -> POSIXTime -> Interval POSIXTime -> Bool
lockPeriodEnded lockPeriod stakedTime validRange =
    intervalContains (interval (stakedTime + lockPeriod) (stakedTime + lockPeriod + 1)) validRange

rewardAmountValid :: Integer -> POSIXTime -> Rational -> Value -> AssetClass -> Bool
rewardAmountValid stakedAmount lockPeriod apr value assetClass =
    assetClassValueOf value assetClass == calculateReward stakedAmount lockPeriod apr

calculateReward :: Integer -> POSIXTime -> Rational -> Integer
calculateReward stakedAmount lockPeriod apr =
    stakedAmount * (numerator apr) `divide` (denominator apr * 365 * 86400) * fromIntegral lockPeriod

-- Compile the validator
stakingValidatorCompiled :: StakingParams -> CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
stakingValidatorCompiled params = $$(compile [|| \d r ctx -> stakingValidator params (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData ctx) ||])

-- Define the staking script
stakingScript :: StakingParams -> Script
stakingScript params = mkValidatorScript (stakingValidatorCompiled params)

-- Define the staking address
stakingAddress :: StakingParams -> Address
stakingAddress params = scriptHashAddress (validatorHash (stakingScript params))

-- Define the staking contract
stakingContract :: StakingParams -> Contract () StakingSchema Text ()
stakingContract params = do
    -- Stake action
    handleStake <- endpoint @"stake" $ \(staker, stakedAmount) -> do
        now <- currentTime
        let datum = StakingDatum { staker = staker, stakedAmount = stakedAmount, stakedTime = now }
        let tx = mustPayToTheScript datum (assetClassValue (stakeToken params) stakedAmount)
        submitTxConstraints (stakingScript params) tx
        awaitTxConfirmed (getCardanoTxId tx)
        logInfo @Text $ "Staked " <> show stakedAmount <> " " <> show (stakeToken params)

    -- Withdraw action
    handleWithdraw <- endpoint @"withdraw" $ \(staker, stakedAmount) -> do
        now <- currentTime
        let redeemer = StakingRedeemer { action = Withdraw }
        let tx = mustSpendScriptOutput (stakingScript params) redeemer
        submitTxConstraints (stakingScript params) tx
        awaitTxConfirmed (getCardanoTxId tx)
        logInfo @Text $ "Withdrawn " <> show stakedAmount <> " " <> show (stakeToken params)

    -- Combine the handlers
    selectList [handleStake, handleWithdraw]

-- Define the schema
type StakingSchema =
    Endpoint "stake" (PubKeyHash, Integer)
    .\/ Endpoint "withdraw" (PubKeyHash, Integer)

-- Define the main function
main :: IO ()
main = do
    -- Define the staking parameters
    let botlyStakeBotlyParams = StakingParams
            { stakeToken = assetClass "BOTLY" "BOTLY"
            , rewardToken = assetClass "BOTLY" "BOTLY"
            , lockPeriod = 30 * 86400  -- 30 days in seconds
            , apr = 3 % 100
            }
    let botlyStakeAdaParams = StakingParams
            { stakeToken = assetClass "BOTLY" "BOTLY"
            , rewardToken = assetClass "" "ADA"
            , lockPeriod = 30 * 86400  -- 30 days in seconds
            , apr = 1.3 % 100
            }
    let botlyStakeSnekParams = StakingParams
            { stakeToken = assetClass "BOTLY" "BOTLY"
            , rewardToken = assetClass "SNEK" "SNEK"
            , lockPeriod = 30 * 86400  -- 30 days in seconds
            , apr = 2.1 % 100
            }
    let cockStakeCockParams = StakingParams
            { stakeToken = assetClass "COCK" "COCK"
            , rewardToken = assetClass "COCK" "COCK"
            , lockPeriod = 30 * 86400  -- 30 days in seconds
            , apr = 15 % 100
            }

    -- Run the staking contracts
    runPlutusApp $ stakingContract botlyStakeBotlyParams
    runPlutusApp $ stakingContract botlyStakeAdaParams
    runPlutusApp $ stakingContract botlyStakeSnekParams
    runPlutusApp $ stakingContract cockStakeCockParams