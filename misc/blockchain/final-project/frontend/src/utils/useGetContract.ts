import { ethers } from "ethers";
import Database  from '../../../backend/artifacts/contracts/Database.sol/Database.json'

export default function getContract(contractAddress: string): any {
  const provider = new ethers.providers.Web3Provider( (window as any).ethereum);
  const signer = provider.getSigner();
  const contract = new ethers.Contract(
    contractAddress,
    Database.abi,
    signer
  );
  return contract;
}