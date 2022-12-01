import { ethers } from "hardhat";

async function main() {
  const NFTCollection = await ethers.getContractFactory("NFTCollection")
  const Rewards = await ethers.getContractFactory("Rewards")
  const NFTStaking = await ethers.getContractFactory("NFTStaking")

  const accounts = await ethers.getSigners()
  const owner = accounts[0]
  const acc1 = accounts[1]

  console.log(`contracts owner: ${owner.address}`)
  console.log(`acc1: ${acc1.address}`)

  const collection1 = await NFTCollection.connect(owner).deploy("Collection1", "C1")

  const nftStaking = await NFTStaking.connect(owner).deploy()
  const rewards1 = await Rewards.connect(owner).deploy()
  
  console.log(`NFT Collection SC: ${collection1.address}`)
  console.log(`Rewards SC: ${rewards1.address}`)
  console.log(`NFT Staking SC: ${nftStaking.address}`)

  await nftStaking.addVault(collection1.address, rewards1.address, "vault1")
  
  // account allows nftstaking operate all account tokens on this collection
  await collection1.connect(acc1).setApprovalForAll(nftStaking.address, true)
  await rewards1.connect(owner).addController(nftStaking.address)

  await collection1.connect(owner).mint(acc1.address, await collection1.maxMintAmount())

  await nftStaking.connect(acc1).stake(0, [1,2,3])
  console.log(`acc1 staked token 1 on vault 0`)
  console.log(`now, balance of acc1  is ${await nftStaking.balanceOf(acc1.address, 0)}`)

}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
