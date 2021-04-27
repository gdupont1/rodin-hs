`rodin-api` -- A Haskell package for dealing with Rodin files and Event-B structures

# Synopsis

`rodin-api` is a basic Haskell library that allows to handle [Event-B](http://www.event-b.org/) component and to parse [Rodin](http://www.event-b.org/install.html) (XML) files.

This API can handle:
 * Machine components (`Rodin.Machine`)
 * Context components (`Rodin.Context`)
 * Proof obligation files (`Rodin.ProofObligations`)
 * Theories (`Rodin.Theory`), components defined an handles via the [<i>Theory plug-in</i>](http://wiki.event-b.org/index.php/Theory_Plug-in), an extension of Event-B

# Install

This repository essentially contains a Cabal package. You can install it with:

    cabal build
    cabal install

You can also build the dock with:
    
    cabal haddock

# Important Packages

The API mainly defines 3 types of modules:
 * A module containing the basic type (`Rodin.Machine`, `Rodin.Context`, ...)
 * A module for parsing a Rodin file into this type (`Rodin.Machine.Read`, `Rodin.Context.Read`, ...)
 * A module for exporting (pretty-printing) a type to ASCII (`Rodin.Machine.Ascii`, `Rodin.Context.Ascii`, ...)

It also defines and exports convenient utilities:
 * `Rodin.Internal.Util`, which are likely to be used by other packages and contain various string-handling functionalities
 * `Rodin.Internal.Wrap`, a wrapping monad used for error and warning handling

Last and for reference, it exposes the `Rodin.Ascii` module, defining what is needed to handle ASCII exporting.

## Event-B Formulas

`rodinapi` exposes various modules related to the parsing and handling of Event-B <i>formulas</i>. These modules are located in `Rodin.Formula.*`, which mainly contains:
 * `Rodin.Formula`, a module defining the type for handling formulas as lists of `Token`s, defined in the same module
 * `Rodin.Formula.Ascii`, a module for exporting formulas to ASCII
 * `Rodin.Formula.UTF8`, a module for exporting formulas to UTF-8, also used to parse tokens of a formula (which are stored as UTF-8 symbols in Rodin files)
 * `Rodin.Formula.Tokenizer`, a module for transforming a UTF-8 string containing an Event-B formula to a list of tokens
 * `Rodin.Formula.Tokenizer.Error`, a module for handling errors when parsing a formula
 * `Rodin.Formula.Internal.Util`, a module defining various utilities for handling formulas, likely used by other packages

## A Note on ASCII export

ASCII export of Event-B structures is mostly informal as Rodin cannot interpret ASCII files. The syntax used for this type of exporting is highly based on the [B-Method](https://www.methode-b.com/) mathematical language.

However, we do not guarantee that Event-B formulas exported to ASCII via this API will yield correct B formulas.

# Dependencies

The XML parsing part is based on the Haskell simple XML library <http://hackage.haskell.org/package/xml>. The library also uses the Haskell transformers library <http://hackage.haskell.org/package/transformers>.

Other than that, it is fairly standalone.

# License

This work is distributed under the GPLv3 license, detailed in the LICENSE file.




