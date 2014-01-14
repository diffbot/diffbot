diffbot
=======

Simple client for the Diffbot API.

## Installation

The easiest way to install the package and its dependencies is to use
the `cabal` command line tool. The
[Cabal-Install](http://www.haskell.org/haskellwiki/Cabal-Install) page
explains how to use `cabal`.

```
$ git clone https://github.com/tymmym/diffbot.git
$ cd diffbot
diffbot $ cabal install diffbot
```

## Usage

### Article API

```haskell
import Diffbot

main = do
    let token = "11111111111111111111111111111111"
        url   = "http://blog.diffbot.com/diffbots-new-product-api-teaches-robots-to-shop-online/"
    resp <- diffbot token url mkArtikle
    print resp
```
