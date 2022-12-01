import { ethers } from "hardhat";


async function main(contractName: string) {
  console.log(`Testing contract ${contractName}`)

  const Collection = await ethers.getContractFactory(contractName)

  const accounts = await ethers.getSigners()
  const owner = accounts[0]

  console.log(`TimeLock owner: ${owner.address}`)
  const collection = await Collection.connect(owner).deploy()

  console.log(`Collection address: ${collection.address}`)

  const initBalance = await owner.getBalance()
  console.log(`Owner's initial balance: ${initBalance}`)

  await collection.deposit({ value: 3000 })
  const initDepositBalance = await collection.balances(owner.address)
  console.log(`Owner's initial deposit balance: ${initDepositBalance}`)

  const afterDepositBalance = await owner.getBalance()
  console.log(`Owner's balance after depositing: ${afterDepositBalance}`)

  const lockTime = await collection.lockTime(owner.address)

  const MAX_INT = ethers.BigNumber.from(115792089237316195423570985008687907853269984665640564039457584007913129639935n)
  const lockTimeIncrease = MAX_INT.sub(lockTime).add(1)

  console.log(`Increase lock time by: ${lockTimeIncrease}`)

  await collection.increaseLockTime(lockTimeIncrease)

  const newLockTime = await collection.lockTime(owner.address)

  console.log(`New lock time: ${newLockTime}`)

  console.log(`Withdrawing`)

  await collection.withdraw()

  const finalDepositBalance = await collection.balances(owner.address)
  console.log(`Owner's final deposit balance: ${finalDepositBalance}`)

  const finalBalance = await owner.getBalance()
  console.log(`Owner's final balance: ${finalBalance}`)

  console.log(`Balance changed by: ${finalBalance.sub(initBalance)}`)
}

const contracts = ["TimeLock", "TimeLockFixed"]

async function runContract(x: string) {
  await main(x).catch((error) => {
    console.error(error);
    process.exitCode = 1;
  })
}

async function run() {
  await runContract(contracts[0])
  console.log("\n\n\n")
  await runContract(contracts[1])
}

run()