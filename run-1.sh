#!/bin/bash
printf "\nThis demo showcases checking for an NFT exists in a wallet.\n"
sleep 1

printf "\n"
export IssuerW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export HolderWA=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export HolderWB=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export CheckerW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1

export IssuerW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "IssuerContract", "caWallet":{"getWalletId": '$IssuerW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export HolderWA_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "HolderContract", "caWallet":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export HolderWB_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "HolderContract", "caWallet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export CheckerW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "CheckerContract", "caWallet":{"getWalletId": '$CheckerW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
printf "\nThere are four wallets. 1 NFT Issuer, 2 NFT holders and 1 NFT Checker\n"
printf "\nThere are 2 categories of NFTs minted in this example - GOLD and SILVER\n"
printf "\nHolder A gets 'GOLD' nft and Holder B gets 'SILVER' nft from the Issuer\n"

sleep 2
printf "\n1. Log the currency symbol for reference.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '[]' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/logCS
sleep 1

sleep 1
printf "\n2. Mint NFTs and send to A and B wallets\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"tn":"GOLD", "destW":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 2
curl -H "Content-Type: application/json" -X POST -d '{"tn":"SILVER", "destW":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 1

printf "\n3. Checker wallet now tries to find GOLD NFT in both wallets. You'll see in pab server logs that NFT is found inside wallet A only.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"nftTokenName":"GOLD", "issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/findNFT
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"nftTokenName":"GOLD", "issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/findNFT
printf "\n"
sleep 1

printf "\n3. Checker wallet now tries to find SILVER NFT in both wallets. You'll see in pab server logs that NFT is found inside wallet B only.\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"nftTokenName":"SILVER", "issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/findNFT
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"nftTokenName":"SILVER", "issuerWallet":{"getWalletId": '$IssuerW'}, "holderWallet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/findNFT
printf "\n"
sleep 1

printf "\nThank you for your time.\n"
printf "\n"


