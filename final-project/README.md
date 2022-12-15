# NFT ERC721 Staking Smart Contract (SC) With ERC20 Token Rewards

## Disclaimer

The work is unfinished

## Run project

1. Install dependencies

  ```sh
  npm i
  ```

1. terminal 1:

    ```sh
    npx hardhat node
    ```

1. terminal 2

    ```sh
    npx hardhat run scripts/deploy.ts --network localhost
    ```

## Solution

The problem was that `increaseLockTime` allows overflows in the original SC.

![img](README/TimeLock.png)

To fix this, I checked for overflow in `increaseLockTime` (see [TimeLockFixed](./contracts/TimeLockFixed.sol))

![img](README/TimeLockFixed.png)

## Sample Hardhat Project

This project demonstrates a basic Hardhat use case. It comes with a sample contract, a test for that contract, and a script that deploys that contract.

Try running some of the following tasks:

```shell
npx hardhat help
npx hardhat test
REPORT_GAS=true npx hardhat test
npx hardhat node
npx hardhat run scripts/deploy.ts
```

## Test

- may need to `npm audit fix --force` - [SO](https://stackoverflow.com/a/73027407)


![GitHub Banner](https://user-images.githubusercontent.com/40567147/159485872-7f63766a-3c91-48dc-aa37-fb5894232acc.png)

This starter template comes as a monorepo for your next fullstack dApp Development. This will be your tools:
:pager: React + Vite + Typescript
 :page_with_curl: Solidity + Hardhat + Typescript

Typescript is integrated in the frontend as well in the smart contract part. This gives you a HUGE advantage, why? Because you can use types from the smart contract in your frontend part ( with the help of typechain https://github.com/dethcrypto/TypeChain). 


## How to start

### Backend
pre: cd into /backend

0) start local testnet ---> npm run testnet
1) Compile contracts ---> npm run build
2) Test contracts -->     npm run test
3) Deploy contracts -->   npm run deploy

### Frontend
pre: cd into /frontend

1) Install dependencies ---> npm install
2) start frontend ---> npm run dev
3) build --> npm run build

Here are some ready to use IPFS services 📡, that you can easily use for your next project 🚀

🔗 https://pinata.cloud  
🔗 https://nft.storage 
🔗 https://docs.moralis.io/moralis-dapp/files/ipfs
🔗 https://infura.io/product/ipfs 

🌞 GM TO ALL OF YOU AND KEEP LEARNING WEB 3 -Johannes (https://twitter.com/XamHans)

PS: If you are looking for a Web3 Job checkout my newest project :green_heart: : https://www.newdevsontheblock.com/ 
