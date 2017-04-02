# How to install cudd
* Download and unpack the cudd tarball
* Configure cudd with `--enable-shared --enable-ddmp --enable-obj`
* Install cudd
* Add `/usr/local/lib` to `$LD_LIBRARY_PATH`
* Copy `util/util.h` and `mtr/mtr.h` to `usr/local/include`

# How to install the cudd haskell library
* Install `c2hs`
* `cabal install cudd --extra-include-dir=/usr/local/include --extra-lib-dir=/usr/local/lib` 
* Use `cabal repl` in the sandbox and make sure `Cudd.Cudd` can be included
