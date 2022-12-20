import { ethers } from "hardhat";


async function main(contractName: string) {
  console.log(`Testing contract ${contractName}`)

  const Contract = await ethers.getContractFactory(contractName)

  const accounts = await ethers.getSigners()
  const owner = accounts[0]

  console.log(`TimeLock owner: ${owner.address}`)
  const contract = (await Contract.connect(owner).deploy()).connect(owner)

  console.log(`Collection address: ${contract.address}`)

  const initBalance = await owner.getBalance()
  console.log(`Owner's initial balance: ${initBalance}`)

  const oneEth = 100000000000000000000n

  await contract.deposit({ value: oneEth })
  
  const initDepositBalance = await contract.balances(owner.address)
  console.log(`Owner's initial deposit balance: ${initDepositBalance}`)

  const lockTime = await contract.lockTime(owner.address)

  const MAX_INT = ethers.BigNumber.from(0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffn)
  const lockTimeIncrease = MAX_INT.sub(lockTime).add(1)

  console.log(`Increase lock time by: ${lockTimeIncrease}`)

  await contract.increaseLockTime(lockTimeIncrease)

  const newLockTime = await contract.lockTime(owner.address)

  console.log(`New lock time: ${newLockTime}`)

  console.log(`Withdrawing`)

  await contract.withdraw()

  const finalDepositBalance = await contract.balances(owner.address)
  console.log(`Owner's final deposit balance: ${finalDepositBalance}`)

  const finalBalance = await owner.getBalance()
  console.log(`Owner's final balance: ${finalBalance}`)

  // FIXME negative change
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