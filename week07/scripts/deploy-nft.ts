import { ethers } from "hardhat";

async function main() {
  const Collection = await ethers.getContractFactory("Collection")
  const Rewards = await ethers.getContractFactory("Rewards")
  const NFTStaking = await ethers.getContractFactory("NFTStaking")

  const collection = await Collection.deploy()
  const rewards = await Rewards.deploy()
  const nftStaking = await NFTStaking.deploy(collection.address, rewards.address)

  await nftStaking.deployed()

  await rewards.addController(nftStaking.address)

  console.log(`Collection: ${collection.address}`)
  console.log(`Rewards: ${rewards.address}`)
  console.log(`NFTStaking: ${nftStaking.address}`)
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
