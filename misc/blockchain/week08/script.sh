git clone https://github.com/near-examples/nft-tutorial.git

cd nft-tutorial

git switch 6.royalty

rustup target add wasm32-unknown-unknown

yarn build

near login

MAIN_ACCOUNT=demago.testnet

NFT_CONTRACT_ID=nft-example.demago.testnet

near create-account $NFT_CONTRACT_ID --masterAccount $MAIN_ACCOUNT --initialBalance 10

near deploy --accountId $NFT_CONTRACT_ID --wasmFile out/main.wasm

near call $NFT_CONTRACT_ID new_default_meta '{"owner_id": "'$NFT_CONTRACT_ID'"}' --accountId $NFT_CONTRACT_ID

near view $NFT_CONTRACT_ID nft_metadata

near call $NFT_CONTRACT_ID nft_mint '{"token_id": "some id", "metadata": {"title": "some title", "description": "some description", "media": "a link to a media file"}, "receiver_id": "'$MAIN_ACCOUNT'"}' --accountId $MAIN_ACCOUNT --amount 0.1

near view $NFT_CONTRACT_ID nft_token '{"token_id": "some id"}'

MAIN_ACCOUNT_2=your-second-account.testnet

near call $NFT_CONTRACT_ID nft_transfer '{"receiver_id": "'$MAIN_ACCOUNT_2'", "token_id": "some id", "approval_id" :0, "memo": "some memo"}' --accountId $MAIN_ACCOUNT --depositYocto 1

# new token

near call $NFT_CONTRACT_ID nft_mint '{"token_id": "cat id", "metadata": {"title": "cute cat", "description": "a cat wearing glasses", "media": "https://ru.freepik.com/free-vector/cute-cool-cat-wearing-glasses-cartoon-vector-icon-illustration-animal-nature-icon-concept-isolated_23104955.htm"}, "receiver_id": "'$MAIN_ACCOUNT'"}' --accountId $MAIN_ACCOUNT --amount 0.1

near view $NFT_CONTRACT_ID nft_token '{"token_id": "cat id"}'

# Send 1

ACCOUNT_1=hdsaleh.testnet

TOKEN_ID="cat1"

near call $NFT_CONTRACT_ID nft_mint '{"token_id": "'"$TOKEN_ID"'", "metadata": {"title": "cute cat", "description": "a cat wearing glasses", "media": "https://ru.freepik.com/free-vector/cute-cool-cat-wearing-glasses-cartoon-vector-icon-illustration-animal-nature-icon-concept-isolated_23104955.htm"}, "receiver_id": "'$MAIN_ACCOUNT'"}' --accountId $MAIN_ACCOUNT --amount 0.1

near call $NFT_CONTRACT_ID nft_transfer '{"receiver_id": "'$ACCOUNT_1'", "token_id": "'"$TOKEN_ID"'", "approval_id" :0, "memo": "some memo"}' --accountId $MAIN_ACCOUNT --depositYocto 1

# Send 2

ACCOUNT_2=enghamzasalem.testnet

TOKEN_ID="cat2"

near call $NFT_CONTRACT_ID nft_mint '{"token_id": "'"$TOKEN_ID"'", "metadata": {"title": "cute cat", "description": "a cat wearing glasses", "media": "https://ru.freepik.com/free-vector/cute-cool-cat-wearing-glasses-cartoon-vector-icon-illustration-animal-nature-icon-concept-isolated_23104955.htm"}, "receiver_id": "'$MAIN_ACCOUNT'"}' --accountId $MAIN_ACCOUNT --amount 0.1

near call $NFT_CONTRACT_ID nft_transfer '{"receiver_id": "'$ACCOUNT_2'", "token_id": "'"$TOKEN_ID"'", "approval_id" :0, "memo": "some memo"}' --accountId $MAIN_ACCOUNT --depositYocto 1
