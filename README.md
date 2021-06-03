# A set of Haskell modules for handling Rodin files

Please refer to the README in each subdirectory for information on each
subdirectory.

**Note:** this Git repository contain a _submodule_. Do not forget to retrieve it as well:
```
> git submodule init
> git submodule update
```


## Installing `rodin-read`

To install the main tool (`rodin-read`) you need `cabal` version 3.4 or higher (but probably lower than 4.x) and GHC (Haskell compiler) version 9.0.1 or higher.

Cabal and GHC are available for various package repository, including Aptitude (without any extra ppa). You can simply do
```
> sudo apt install cabal-install-3.4 ghc-9.0.1
```

Once you have cloned this repo, you can build its content with
```
> cabal build all
```

You can then install `rodin-read` with
```
> cabal install rodin-read
```

This will install `rodin-read` in `~/.cabal/bin/rodin-read`. You can specify a different installation directory with the `--installdir` option:
```
> cabal install rodin-read --installdir=$HOME/mybins/
```

Note that Cabal uses symlinks to do installation. You can ask Cabal for a hard copy when installing using the `--install-method=copy` option:
```
> cabal install rodin-read --install-method=copy --installdir=$HOME/mybins/
```


## Installing libraries

If you want to play around with `rodin-api` and `rodin-tex` you can install them as well. They expose a number of modules (see documentation in each subdirectory).




