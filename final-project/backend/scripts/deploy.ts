// We require the Hardhat Runtime Environment explicitly here. This is optional
// but useful for running the script in a standalone fashion through `node <script>`.
//
// When running the script with `npx hardhat run <script>` you'll find the Hardhat
// Runtime Environment's members available in the global scope.
// import { ethers } from "hardhat";

// async function main() {
//   // Hardhat always runs the compile task when running scripts with its command
//   // line interface.
//   //
//   // If this script is run directly using `node` you may want to call compile
//   // manually to make sure everything is compiled
//   // await hre.run('compile');

//   // We get the contract to deploy
//   const ContractToDeploy = await ethers.getContractFactory("ExampleContract");
//   const ContractDeployed = await ContractToDeploy.deploy();

//   await ContractDeployed.deployed();

//   console.log("deployed to:", ContractDeployed.address);
// }

// // We recommend this pattern to be able to use async/await everywhere
// // and properly handle errors.
// main().catch((error) => {
//   console.error(error);
//   process.exitCode = 1;
// });


import { ethers } from "hardhat";
import { expect, } from "chai";

async function main() {
  const Database = await ethers.getContractFactory("Database")

  const accounts = await ethers.getSigners()
  const owner = accounts[0]

  console.log(`Database admin: ${owner.address}`)

  const db = await Database.deploy()

  console.log(`Database address: ${db.address}`)

  // await db.CREATE_TABLE("films", ["title", "year"])

  // await db.INSERT("films", ["title", "year"], ["film1", "2000"])
  // await db.INSERT("films", ["title", "year"], ["film2", "2010"])

  // const sel1 = db.SELECT("films", ["year","title"])

  // // console.log(await sel1)
  // const res = await (await sel1).wait()
  // console.log(res.events[0]?.args[0])
  
  // console.log(sel1)

  // console.log(sel1)

  // const data1 = (await db.SELECT("films", ["title"])).data
  // console.log(data1)

  // const data = db.interface.decodeFunctionData("SELECT", data1)

  // console.log(db.interface.fragments)

  // console.log(data)
}

// We recommend this pattern to be able to use async/await everywhere
// and properly handle errors.
main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});
