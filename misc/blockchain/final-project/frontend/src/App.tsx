import { Container, Box, Stack, TextField, Button, Grid, buttonClasses, Tab, Tabs } from "@mui/material";

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
import { safeParse2dArray, safeParseArray } from "./Parser";
import { styled } from '@mui/material/styles';
import Paper from '@mui/material/Paper';

declare let window: any

const Item = styled(Paper)(({ theme }) => ({
  backgroundColor: theme.palette.mode === 'dark' ? '#1A2027' : '#fff',
  ...theme.typography.body2,
  padding: theme.spacing(1),
  textAlign: 'center',
  color: theme.palette.text.secondary,
}));


function App() {


  /*-----------STATES---------------*/
  const { connect, metaState, getAccounts } = useMetamask()
  const [tableName, setTableName] = useState("")
  const [columns, setColumns] = useState("")
  const [values, setValues] = useState("")
  const [account, setAccount] = useState("")
  const [response, setResponse] = useState(<Box>No data :)</Box>)
  const [query, setQuery] = useState(<Box>Query:</Box>)
  const [isValuesInputValid, setValuesInputValid] = useState(false)
  const [isColumnsInputValid, setColumnsInputValid] = useState(false)
  const [isQueryValid, setQueryValid] = useState(false)
  const [isTableNameValid, setTableNameValid] = useState(false)
  const [tabIndex, setTabIndex] = useState(0);
  const contractAddress = "0x5FbDB2315678afecb367f032d93F642f64180aa3"
  const [typedContract, setTypedContract] = useState<Database>()


  /*-----------SIDE EFFECTS---------------*/
  // useEffect(() => {
  //   if (!window.ethereum) return
  //   const provider = new ethers.providers.Web3Provider(window.ethereum)
  //   const erc20 = new ethers.Contract(contractAddress, abi, provider)
  //   // setTypedContract(getContract(contractAddress) as Database)
  // }, [])

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
          let account_ = await getAccounts();
          setAccount(account_)
        } catch (error) {
          console.log(error);
        }
      })();
    }
  }, []);

  useEffect(() => {
    const cv = columns.length == values.length
    const civ = isColumnsInputValid
    const viv = isValuesInputValid
    const res = isTableNameValid &&
      (
        (tabIndex == 0 && civ) ||
        (tabIndex == 1 && civ) ||
        (tabIndex == 2 && civ && viv && cv) ||
        (tabIndex == 3)
        // (tabIndex == 3 && civ && viv && cv) ||
      )
    setQueryValid(res)
  }, [tabIndex, tableName, columns, values])

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

  function mkColumns(columns_: string): string[] {
    const res = safeParseArray(columns_)
    setColumnsInputValid(!res.hasError)
    return res.hasError ? [] : res.parsed
  }

  function mkValues(values_: string): string[][] {
    const res = safeParse2dArray(values_)
    setValuesInputValid(!res.hasError)
    return res.hasError ? [[]] : res.parsed
  }


  function mkColumnsRow(columns_: string): string {
    const p = safeParseArray(columns_)
    const res = p.hasError ? [] : p.parsed
    return `(${res.join(", ")})`
  }

  function mkColumnsColumn(columns_: string): string {
    const p = safeParseArray(columns_)
    const res = p.hasError ? [] : p.parsed
    return `(
      ${res.map(x => `  ${x}  string`).join(",\n")}
    )`
  }

  function mkWhere(columns_: string, values_: string) {
    const result = [];
    for (let i = 0; i < Math.min(columns_.length, values_.length); i++) {
      result.push(`${columns_[i]}=${values_[i]}`);
    }
    return result.join(", ")
  }

  function checkColumnsValid(columns_: string) {
    mkColumns(columns_)
  }

  function checkValuesValid(values_: string) {
    mkValues(values_)
  }

  function isAlpha(ch: string) {
    return ch.length === 1
      && (ch >= "a" && ch <= "z" || ch >= "A" && ch <= "Z");
  }

  function checkTableNameValid(name_: string) {
    setTableNameValid(name_.length > 0 && isAlpha(name_[0]))
  }

  const tableNamePlaceholder = `table_one`
  const columnsPlaceholder = `["column_one", "column_two"]`
  const valuesPlaceholder = `["value_one", "value_two"]`

  const columnsInput =
    <TextField id="filled-basic"
      label="Column names"
      variant="filled"
      name="column_names"
      placeholder={columnsPlaceholder}
      value={columns}
      onChange={(e) => {
        setColumns(e.target.value)
        checkColumnsValid(e.target.value)
      }}
      error={!isColumnsInputValid}
    />

  const tableNameInput =
    <TextField id="filled-basic"
      label="Table name"
      variant="filled"
      name="table_name"
      placeholder={tableNamePlaceholder}
      value={tableName}
      onChange={(e) => {
        setTableName(e.target.value)
        checkTableNameValid(e.target.value)
      }}
      error={!isTableNameValid}
    />

  const valuesInput =
    <TextField id="filled-basic"
      label="Values"
      variant="filled"
      name="values"
      placeholder={valuesPlaceholder}
      value={values}
      multiline
      error={!isValuesInputValid}
      onChange={
        (e) => {
          setValues(e.target.value)
          checkValuesValid(e.target.value)
        }
      }
    />

  const handleTabChange = (event, newTabIndex: number) => {
    setTabIndex(newTabIndex);
  };

  return (
    <Grid container spacing={2}>
      <Grid item xs={6} md={6}>
        <Item>
          <Grid id="metamask">
            <Item>
              <Box sx={{ width: '100%', typography: 'body1', display: 'flex' }}>
                <FormControl sx={{ m: 3 }} component="fieldset" variant="standard">
                  <FormLabel component="legend">Metamask</FormLabel>
                  <FormGroup aria-label="position">
                    <FormControlLabel
                      value="end"
                      control={<Checkbox />}
                      label="Available"
                      labelPlacement="end"
                      checked={metaState.isAvailable}
                    />
                    <FormControlLabel
                      value="end"
                      control={<Checkbox />}
                      label={`Connected account: ${account}`}
                      labelPlacement="end"
                      checked={metaState.isConnected}
                    />
                  </FormGroup>
                </FormControl>
              </Box>
            </Item>
          </Grid>
        </Item>
        <Item>
          <Grid container id="queries">
            <Grid item id="queries-inputs" xs={6}>
              <Item>
                <Box sx={{ width: '100%', typography: 'body1', display: 'flex' }}>
                  <Tabs
                    value={tabIndex}
                    onChange={handleTabChange}
                    orientation="vertical"
                  >
                    <Tab label="CREATE TABLE" />
                    <Tab label="SELECT * FROM" />
                    <Tab label="INSERT INTO" />
                    {/* <Tab label="DELETE FROM" /> */}
                    <Tab label="DROP TABLE" />
                  </Tabs>
                  <Box sx={{ margin: 2 }}>
                    {tabIndex === 0 && (
                      <Box>
                        <Stack direction="column" alignItems="left" spacing={2} mb={4}>
                          {tableNameInput}
                          {columnsInput}
                        </Stack>
                      </Box>
                    )}
                    {tabIndex === 1 && (
                      <Box>
                        <Stack direction="column" alignItems="left" spacing={2} mb={4}>
                          {tableNameInput}
                          {columnsInput}
                        </Stack>
                      </Box>
                    )}
                    {tabIndex === 2 && (
                      <Box>
                        <Stack direction="column" alignItems="left" spacing={2} mb={4}>
                          {tableNameInput}
                          {columnsInput}
                          {valuesInput}
                        </Stack>
                      </Box>
                    )}
                    {tabIndex === 3 && (
                      <Box>
                        <Stack direction="column" alignItems="left" spacing={2} mb={4}>
                          {tableNameInput}
                        </Stack>
                      </Box>
                    )}
                    {/* {tabIndex === 3 && (
                      <Box>
                        <Stack direction="column" alignItems="left" spacing={2} mb={4}>
                          {tableNameInput}
                          {columnsInput}
                          {valuesInput}
                        </Stack>
                      </Box>
                    )} */}
                  </Box>
                </Box>
              </Item>
            </Grid>
            <Grid item id="queries-button-message" xs={6}>
              <Grid item id="button-query">
                <Item>
                  <Button
                    variant="contained"
                    component="span"
                    color={isQueryValid ? "success" : "error"}
                    disabled={!isQueryValid}
                  >
                    {isQueryValid ? "run query" : "check your query"}
                  </Button>
                </Item>
              </Grid>
              {isQueryValid &&
                <Grid item id="full-query">
                  <Item>
                    {tabIndex === 0 && `CREATE TABLE ${tableName} ${mkColumnsColumn(columns)}`}
                    {tabIndex === 1 && `SELECT ${mkColumnsRow(columns)} FROM ${tableName}`}
                    {tabIndex === 2 && `INSERT INTO ${tableName} ${mkColumnsRow(columns)} VALUES ${mkColumnsRow(values)}`}
                    {tabIndex === 3 && `DROP TABLE ${tableName}`}
                    {/* {tabIndex === 3 && `DELETE FROM ${tableName} WHERE ${mkWhere(columns, values)}`} */}
                  </Item>
                </Grid>
              }
            </Grid>
          </Grid>
        </Item>
      </Grid>
      <Grid item xs={6} md={6}>
        <Item>
          Here should be the result of a transaction, but I didn't manage to connect to my SC
          {/* {typedContract.address} */}
        </Item>
      </Grid>
    </Grid>
  );
}

export default App;