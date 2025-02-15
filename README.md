﻿# staking-v1
## Staking Contract Components

### StakingParams
Defines the parameters for each staking option, including the staked token, reward token, lock period, and APR.

### StakingDatum
Stores the staker's public key hash, the staked amount, and the staking time.

### StakingRedeemer
Defines the actions (Stake or Withdraw) that can be performed on the staking contract.

### stakingValidator
The core logic of the staking contract, ensuring that staking and withdrawal conditions are met.

### Helper Functions
Functions to validate the staking period, staked amount, and reward amount.

### stakingContract
The main contract that handles staking and withdrawal actions.

## Staking Options

### Stake BOTLY Earn BOTLY
- **30 Days**: 3% APR
- **60 Days**: 5% APR
- **90 Days**: 7% APR
- **120 Days**: 9% APR
- **180 Days**: 12% APR
- **365 Days**: 14.5% APR

### Stake BOTLY Earn ADA
- **30 Days**: 1.3% APR
- **60 Days**: 2.59% APR
- **90 Days**: 2.99% APR
- **120 Days**: 3.9% APR
- **180 Days**: 5.2% APR
- **365 Days**: 6.5% APR

### Stake COCK Earn COCK
- **30 Days**: 15% APR
- **60 Days**: 20% APR
- **90 Days**: 19% APR
- **120 Days**: 22% APR
- **180 Days**: 25% APR
- **365 Days**: 30% APR

### Stake BOTLY Earn SNEK
- **30 Days**: 2.1% APR
- **60 Days**: 2.9% APR
- **90 Days**: 3.3% APR
- **120 Days**: 3.9% APR
- **180 Days**: 4.4% APR
- **365 Days**: 4.9% APR
