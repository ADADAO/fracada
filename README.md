<h1 align="center">
  Fracada
</h1>
<p align="center">Plutus dApp which enables users to fractionalize their NFTs.</p>

<p align="center"><img src="https://img.shields.io/badge/license-mit-blue?style=for-the-badge&logo=none" alt="license" /></p>

## Disclaimer

The code on this repository has **not** been audited. We don't recommend it using in production without a full security audit. Use it at your own risk!.

## Protocol

This contract locks an NFT and mints a number of tokens representing fractions of it. To get the NFT back, the fraction tokens are burned.

The protocol has three steps:

1. Locking the NFT: The NFT is paid to the contract
2. Mint tokens: Fraction tokens are minted (must be run by the same person who performed step 1).
3. Return the NFT: Burning all the fraction tokens will allow the user to redeem the NFT back.
