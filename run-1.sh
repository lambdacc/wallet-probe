#!/bin/bash
printf "\nCheck if an NFT exists in a wallet.\n"
sleep 1

printf "\n"
export IssuerW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
export HolderWA=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
export HolderWB=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
export CheckerW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`

export IssuerW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "IssuerContract", "caWallet":{"getWalletId": '$IssuerW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export HolderWA_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "IssuerContract", "caWallet":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export HolderWB_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "IssuerContract", "caWallet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export CheckerW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "CheckerContract", "caWallet":{"getWalletId": '$CheckerW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
printf "\nThere are four wallets. 1 NFT Issuer, 2 NFT holders and 1 NFT Checker\n"
printf "\nThere are 2 categories of NFTs minted in this example - GOLD and SILVER\n"
printf "\nHolder A gets GOLD and HolderB gets SILVER from the Issuer\n"
printf "\nThe currency symbol is the same as the issuer here is the same\n"

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
curl -H "Content-Type: application/json" -X POST -d '{"tn":"GOLD", "w":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
curl -H "Content-Type: application/json" -X POST -d '{"tn":"SILVER", "w":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 1

printf "\n3. Checker wallet now tries to find GOLD NFT in both wallets\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"nftTokenName":"GOLD", "issuerWallet":{"getWalletId": '$IssuerW'}, "issuerWallet":{"getWalletId": '$HolderWA'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/mint
sleep 1
curl -H "Content-Type: application/json" -X POST -d '{"nftTokenName":"GOLD", "issuerWallet":{"getWalletId": '$IssuerW'}, "issuerWallet":{"getWalletId": '$HolderWB'}}' http://localhost:9080/api/contract/instance/$CheckerW_IID/endpoint/mint
printf "\n"
sleep 1

printf "\nThank you for your time.\n"
printf "\n"


