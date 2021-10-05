# wallet-probe

Check if an NFT exists in a wallet. 

This repo comprises
- smart contracts code with relevant endpoints
- configuration for building PAB executable
- A `run.sh` script using which you can run a demo of the functionality in the PAB simulator

This project gives uses the [Plutus Platform starter project](https://github.com/input-output-hk/plutus-starter) as the template.

### Setting up
Please refer to [Setting up](https://github.com/input-output-hk/plutus-starter#setting-up) section of the plutus starter project.

### The Plutus Application Backend (PAB)

With the PAB we can serve and interact with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with three contracts, `Issuer`, `Holder` and  `Checker` contracts under `/src` folder.


### Steps to run the demo

1. Perform the setup for **Plutus** as described in the plutus starter project.
2. Clone this repo to your local environment.
3. Start a nix shell from the plutus repo from the first step.
4. Build the PAB executable:
```
cabal build plutus-starter-pab
```
5. Run the PAB binary:
 ```
cabal exec -- plutus-starter-pab
````
6. Now, execute `run.sh` for the demo. You might need to do `chmod +x run.sh` first in case execute permissions are missing.

### Support/Issues/Community

If you're looking for support, or would simply like to report a bug, feature
request, etc. please do so over on the main [plutus repository](https://github.com/input-output-hk/plutus).


Thanks!