import { Container, Box, Stack, TextField, Button, Grid, buttonClasses } from "@mui/material";
import React, { ReactElement, useEffect, useState } from "react";
// if you have successfuly compiled the smart contract in the backend folder, typechain should have created an interface that we can use here 
import { Database } from '../../backend/typechain-types/Database';
import getContract from "./utils/useGetContract";
import { useMetamask } from "use-metamask";
import { ethers } from "ethers";

import Checkbox from '@mui/material/Checkbox';
import FormGroup from '@mui/material/FormGroup';
import FormControlLabel from '@mui/material/FormControlLabel';
import FormControl from '@mui/material/FormControl';
import FormLabel from '@mui/material/FormLabel';


function App() {


  /*-----------STATES---------------*/
  const { connect, metaState, getChain, getAccounts } = useMetamask()

  // const [typedContract, setTypedContract] = useState<Database>()
  // const [contract, setContract] = useState()

  // class MetamaskState {
  //   isAvailable: boolean
  //   isConnected: boolean
  // }

  // class Buttons {
  //   metamask: MetamaskButtons
  // }

  // class State {
  //   buttons: Buttons
  // }

  // let initState: State = {
  //   buttons: {
  //     metamask: {
  //       isAvailable: false,
  //       isConnected: false
  //     }
  //   }
  // }

  // const [metamaskState, meta] = useState<MetamaskState>({ isAvailable: false, isConnected: false })
  // const [selectedImage, setSelectedImage] = useState()
  // const [p, updP] = useState(0)
  // const [candidateFormData, setCandidateFormData] = useState({ name: '', imageHash: '' })
  // const [selectResults, updataSelectResults] = useState([])
  // const contractAddress = "0x5FbDB2315678afecb367f032d93F642f64180aa3"

  /*-----------SIDE EFFECTS---------------*/
  useEffect(() => {
    if (!metaState.isConnected) {
      (async () => {
        try {
          await connect(ethers.providers.Web3Provider, "any");
        } catch (error) {
          console.log(error);
        }
      })();
    }
  }, []);

  useEffect(() => {
    if (metaState.isAvailable) {
      (async () => {
        try {
          let account = await getAccounts();
          console.log(account)
        } catch (error) {
          console.log(error);
        }
      })();
    }
  }, []);

  // useEffect(() => {
  //   setTypedContract(getContract(contractAddress) as Database)
  // }, [])

  // useEffect(() => {
  //   upd
  // })


  /*-----------FUNCTIONS---------------*/
  // async function addAccount() {
  // if you want to use the typed contract

  // await (await typedContract?.addAccount("XamHans"));
  //   typedContract?.on("accountCreatedEvent", async function (event:any) {
  //     console.log('Recieved event from smart contract ',event)
  // }),

  // await contract.addAccount("XamHans");
  // contract?.on("accountCreatedEvent", async function (event: any) {
  //   console.log('Recieved event from smart contract ', event)
  // })

  //   console.log("Some click")
  // }

  return (
    <Box sx={{ display: 'flex' }}>
      <FormControl sx={{ m: 3 }} component="fieldset" variant="standard">
        <FormLabel component="legend">Metamask</FormLabel>
        <FormGroup aria-label="position" row>
          <FormControlLabel
            value="end"
            control={<Checkbox />}
            label="available"
            labelPlacement="end"
            checked={metaState.isAvailable}
            color="success"
          />
          <FormControlLabel
            value="end"
            control={<Checkbox />}
            label="connected"
            labelPlacement="end"
            checked={metaState.isConnected}
            color="success"
          />
        </FormGroup>
      </FormControl>
      <Container maxWidth="md" sx={{ marginY: "2rem" }}>
        <Box component="form">
          <Stack direction="row" alignItems="center" spacing={2} mb={4}>

            <TextField id="filled-basic"
              label="Name" variant="filled"
              name="name"
              value={metaState.isConnected}
            // onChange={() => updP(p - 1)}
            />
            <TextField id="filled-basic"
              label="Name" variant="filled"
              name="name"
              value={metaState.isAvailable}
            />
            <Button variant="contained" component="span"
            // onClick={() => updP(p + 1)}
            >
              Register as Candidate
            </Button>
          </Stack>
        </Box>
      </Container>
    </Box>
  );
}

export default App;