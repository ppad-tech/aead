# aead

[![](https://img.shields.io/hackage/v/ppad-aead?color=blue)](https://hackage.haskell.org/package/ppad-aead)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-aead-lightblue)](https://docs.ppad.tech/aead)

A pure Haskell implementation of authenticated encryption with
associated data (AEAD) using the ChaCha20-Poly1305 configuration, as
specified by [RFC8439][8439].

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  > import qualified Data.ByteString.Base16 as B16 -- just for illustration
  >
  > -- import qualified
  > import qualified Crypto.AEAD.ChaCha20Poly1305 as AEAD
  >
  > -- encrypt plaintext with some additional authenticated data, using
  > -- a secret key and nonce
  > let key = "don't tell anyone my secret key!"
  > let non = "or my nonce!"
  > let msg = "this is my secret message"
  > let aad = "and i approve it"
  >
  > -- encryption produces a 128-bit MAC
  > let Right (cip, mac) = AEAD.encrypt aad key non msg
  > B16.encode cip
  "d6377eab18cad56e8c6176968460e6a548c524b9498c9b993e"
  > B16.encode mac
  "48751cc57cf5123bc841239c7d563da0"
  >
  > -- supply both to decrypt
  > AEAD.decrypt aad key non (cip, tag)
  Right "this is my secret message"
  >
  > -- bogus MACs will cause decryption to fail
  > AEAD.decrypt aad key non (cip, "really i swear!!")
  Left InvalidMAC
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/aead][hadoc].

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code.

Current benchmark figures on a simple input from the RFC8439 appendices
on an M4 Silicon MacBook Air look like (use `cabal bench` to run the
benchmark suite):

```
  benchmarking ppad-aead/encrypt
  time                 2.589 μs   (2.587 μs .. 2.592 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 2.595 μs   (2.594 μs .. 2.597 μs)
  std dev              5.384 ns   (4.491 ns .. 6.511 ns)

  benchmarking ppad-aead/decrypt
  time                 2.625 μs   (2.618 μs .. 2.636 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 2.614 μs   (2.610 μs .. 2.621 μs)
  std dev              15.99 ns   (11.98 ns .. 27.52 ns)
```

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be [challenging to achieve][const].

The AEAD-ChaCha20-Poly1305 implementation within passes all
test vectors from RFC8439, as well as the available [Project
Wycheproof vectors][wyche], using the ChaCha20 cipher from
[ppad-chacha](https://github.com/ppad-tech/chacha) and the Poly1305
MAC from [ppad-poly1305](https://github.com/ppad-tech/poly1305),
respectively.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-aead
```

to get a REPL for the main library.

[8439]: https://datatracker.ietf.org/doc/html/rfc8439
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[hadoc]: https://docs.ppad.tech/aead
[const]: https://www.chosenplaintext.ca/articles/beginners-guide-constant-time-cryptography.html
[wyche]: https://github.com/C2SP/wycheproof
