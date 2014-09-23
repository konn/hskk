hSKK
============
CAUTION: λSKK is currently alpha version. USE AT YOUR OWN RISK.

λSKK is the (Not yet) SKK implementation with Haskell + FRP (ordrea) + InputMethodKit.

Build
=====
CAUTION: You need [the most rescent `language-c-inline`](https://github.com/mchakravarty/language-c-inline/tree/def179b26c104cdfa73e1e873214aa5ef600f531) from GitHub to compile.

```sh
$ cabal-install shake

$ ... [install most rescent language-c-inline from GitHub]

$ cabal-install --only-dependencies
$ ./Builder.hs
$ sudo cp hSKK.app /Library/Input\ Methods/
```
