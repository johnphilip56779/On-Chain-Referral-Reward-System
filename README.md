# 🎯 On-Chain Referral Reward System

A complete Clarity smart contract implementation for managing referral programs on the Stacks blockchain. Track referrals, distribute rewards, and build viral growth mechanisms directly on-chain! 🚀

## ✨ Features

- 👥 **User Registration** - Register users with unique referral codes
- 🔗 **Referral Tracking** - Automatic tracking of who referred whom
- 💰 **Reward Distribution** - STX rewards for successful referrals
- 📊 **Statistics** - Real-time stats on users, referrals, and rewards
- 🛡️ **Admin Controls** - Owner functions for system management
- 💸 **Claim System** - Users can claim their earned rewards

## 🏗️ Contract Architecture

### Data Structures
- **Users Map** - Stores user profiles with referral codes and stats
- **Referral Codes Map** - Maps codes to user addresses
- **Reward Tracking** - Tracks available and claimed rewards
- **History Map** - Complete referral transaction history

### Key Constants
- Base referral code starts at: `1000000`
- Default reward per referral: `1,000,000 μSTX` (1 STX)
- Error codes for all edge cases

## 🚀 Getting Started

### Prerequisites
- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

1. Clone the repository:
```bash
git clone <your-repo-url>
cd On-Chain-Referral-Reward-System
```

2. Install dependencies:
```bash
npm install
```

3. Check the contract:
```bash
clarinet check
```

4. Run tests:
```bash
clarinet test
```

## 📖 Usage Guide

### 🔧 Admin Functions

#### Fund the Contract
```clarity
(contract-call? .on-chain-referral-reward-system fund-contract u10000000)
```

#### Set Reward Amount
```clarity
(contract-call? .on-chain-referral-reward-system set-reward-amount u2000000)
```

#### Withdraw Funds
```clarity
(contract-call? .on-chain-referral-reward-system withdraw-funds u5000000)
```

### 👤 User Functions

#### Register Without Referral
```clarity
(contract-call? .on-chain-referral-reward-system register-user none)
```

#### Register With Referral Code
```clarity
(contract-call? .on-chain-referral-reward-system register-user (some u1000000))
```

#### Claim Rewards
```clarity
(contract-call? .on-chain-referral-reward-system claim-rewards)
```

### 📊 Read-Only Functions

#### Get User Info
```clarity
(contract-call? .on-chain-referral-reward-system get-user-info 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

#### Get Total Statistics
```clarity
(contract-call? .on-chain-referral-reward-system get-total-stats)
```

#### Check User's Referral Count
```clarity
(contract-call? .on-chain-referral-reward-system get-user-referrals 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 🔄 Typical Workflow

1. **🏦 Admin Setup**
   - Deploy contract
   - Fund contract with STX
   - Set reward amounts

2. **👥 User Onboarding**
   - First user registers without referral code
   - Gets unique referral code (e.g., 1000000)
   - Shares code with friends

3. **🎯 Referral Process**
   - New users register with referral code
   - Referrer automatically gets rewards
   - System tracks all relationships

4. **💎 Reward Claims**
   - Users check available rewards
   - Claim rewards when ready
   - STX transferred to their wallet

## 🎮 Example Scenario

```clarity
;; Alice registers first
(contract-call? .on-chain-referral-reward-system register-user none)
;; Returns: (ok u1000000) - Alice's referral code

;; Bob uses Alice's code
(contract-call? .on-chain-referral-reward-system register-user (some u1000000))
;; Returns: (ok u1000001) - Bob's referral code
;; Alice automatically receives rewards

;; Alice claims her rewards
(contract-call? .on-chain-referral-reward-system claim-rewards)
;; Alice receives STX in her wallet
```

## 📋 Error Codes

| Code | Description |
|------|-------------|
| `u1000` | Not authorized |
| `u1001` | User already registered |
| `u1002` | User not found |
| `u1003` | Invalid referral code |
| `u1004` | Cannot refer self |
| `u1005` | Insufficient balance |
| `u1006` | Reward already claimed |
| `u1007` | No rewards available |

## 🧪 Testing

Run the test suite:
```bash
clarinet test
```

Check contract syntax:
```bash
clarinet check
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is licensed under the MIT License.

## 🛠️ Built With

- **Clarity** - Smart contract language for Stacks
- **Clarinet** - Development environment and testing framework
- **Stacks Blockchain** - Decentralized computing platform

---

*Built with ❤️ for the Stacks ecosystem*
