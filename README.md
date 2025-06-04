# 🏦 NestNFT - Pension NFT Tracker

## 📋 Overview

NestNFT is a revolutionary blockchain-based solution that represents retirement benefits as Non-Fungible Tokens (NFTs). This smart contract allows employers to mint pension benefits as tradeable digital assets, providing transparency, portability, and liquidity to traditional retirement plans.

## ✨ Features

- 🎯 **Mint Pension NFTs**: Create unique tokens representing retirement benefits
- 💰 **Track Valuations**: Monitor pension values with automated calculations  
- 🔄 **Transfer Benefits**: Enable portable pension benefits between employers
- ⏰ **Vesting Management**: Handle vesting schedules and eligibility
- 📊 **Claim Tracking**: Monitor claimed vs unclaimed benefits
- 📈 **Portfolio Analytics**: View comprehensive pension statistics

## 🚀 Getting Started

### Prerequisites

- Clarinet CLI installed
- Stacks wallet for testing
- Basic understanding of Clarity smart contracts

### Installation

```bash
git clone <your-repo>
cd nestnft-project
clarinet check
```

## 🔧 Usage

### Minting a Pension NFT

Only the contract owner can mint new pension NFTs:

```clarity
(contract-call? .Nestnft mint-pension-nft 
    'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM  ;; recipient
    "Acme Corp"                                    ;; employer
    u2500                                         ;; monthly benefit ($25.00)
    u1000                                         ;; vesting date (block height)
    u65                                           ;; retirement age
    u10                                           ;; contribution years
    "defined-benefit"                             ;; pension type
)
```

### Transferring Pension Benefits

```clarity
(contract-call? .Nestnft transfer 
    u1                                            ;; token ID
    tx-sender                                     ;; current owner
    'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG    ;; new owner
)
```

### Claiming Pension Benefits

```clarity
(contract-call? .Nestnft claim-pension u1)
```

### Checking Pension Status

```clarity
(contract-call? .Nestnft get-pension-metadata u1)
(contract-call? .Nestnft is-pension-vested u1)
(contract-call? .Nestnft get-pension-valuation u1)
```

## 📊 Read-Only Functions

- `get-pension-metadata(token-id)` - Get complete pension details
- `get-employee-pensions(employee)` - List all pensions for an employee
- `get-pension-valuation(token-id)` - Get current valuation data
- `calculate-pension-value(token-id)` - Calculate estimated pension value
- `get-contract-stats()` - Get overall contract statistics
- `is-pension-vested(token-id)` - Check vesting status
- `is-pension-claimed(token-id)` - Check claim status

## 🏗️ Contract Architecture

### Data Structures

- **pension-metadata**: Core pension information (employer, benefits, dates)
- **pension-valuations**: Current market valuations and methods
- **employee-pensions**: Mapping of employees to their pension NFTs
- **pension-transfers**: Transfer history and pricing data

### Key Constants

- `err-owner-only (u100)` - Only contract owner can perform action
- `err-not-token-owner (u101)` - Caller doesn't own the token
- `err-pension-not-vested (u107)` - Pension hasn't vested yet
- `err-pension-already-claimed (u108)` - Benefits already claimed

## 🧪 Testing

```bash
clarinet test
```

Run the test suite to verify all contract functionality works as expected.

## 🔐 Security Considerations

- Only contract owner can mint new pension NFTs
- Pension claims require proper vesting and ownership verification
- Transfer history is permanently recorded on-chain
- Vesting dates are enforced at the blockchain level

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License.

## 🆘 Support

For questions or issues, please open a GitHub issue or contact the development team.


