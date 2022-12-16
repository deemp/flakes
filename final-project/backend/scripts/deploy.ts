import { ethers } from "hardhat";

async function main() {
  const Database = await ethers.getContractFactory("Database")

  const accounts = await ethers.getSigners()
  const owner = accounts[0]

  console.log(`Database admin: ${owner.address}`)

  const db = (await Database.connect(owner).deploy())

  console.log(`Database deployed to: ${db.address}`)

  await db.CREATE_TABLE("films", ["title", "year"])

  await db.INSERT("films", ["title", "year"], ["film1", "2000"])
  await db.INSERT("films", ["title", "year"], ["film2", "2010"])

  const res = await (await db.SELECT("films", ["year", "title"])).wait()
  console.log(res.events[0]?.args[0])

  const res1 = (await (await db.SELECT("films", ["title"])).wait()).events[0]?.args[0]
  console.log(res1)

  await db.DROP_TABLE("films")
  await db.CREATE_TABLE("films", ["title", "year", "budget"])
  
  const res2 = (await (await db.SELECT("films", ["title"])).wait()).events[0]?.args[0]
  console.log(res2)
  
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
