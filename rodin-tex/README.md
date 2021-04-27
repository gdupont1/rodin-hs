`rodin-tex` -- A Rodin API extension for exporting Event-B structures to LaTeX

# Synopsis

`rodin-tex` is a Rodin API extension providing modules for exporting Event-B structures to LaTeX code.

The API can, among other things, "pretty-print" Event-B formulas to LaTeX with the correct symbols, and it can export components to complete files that can be imported using the `listings` LaTeX package.

# Install

This repository essentially contains a Cabal package. You can install it with:

    cabal build
    cabal install

You can also build the dock with:
    
    cabal haddock

# Important Packages

The API exposes a `Rodin.TeX` module that contains the `Rodin.TeX.ShowTeX` typeclass (used to denote types that can be exported to TeX). This module also defines a few utility functions for handling LaTeX code (such as escaping, bold and italic commands and so on).

Then for each component (e.g. `Rodin.Machine`) the API exposes a `ShowTeX` instance in `*.TeX` modules (e.g. `Rodin.Machine.TeX`).

# Use

The API (supposedly) generates valid LaTeX code, but this code does not work in "standalone". Formulas are generated in math mode and use special characters for Event-B specific operators (such as partial surjection, injection, bijection, etc.). These characters are defined in the first part of the file `latex/rodinlisting.tex`. Other packages such as `amsmath` are needed as well.

As for the other types (`Machine`, `Context`, `Theory` and `ProofObligation`), the result of their LaTeX transformation is a file that can be imported using
    
    \lstinputlisting{<file>.tex}

The only constraints for this file to be successfully used as so is that `listings` is configured so that math mode auto escapes (`mathescape` option) and that it is possible to escape LaTeX code in listings using `(* ... *)` (`escapeinside` option). Other than that, we strongly advise you to import `latex/rodinlisting.tex`, which contains the definition of a new language, `eventb`, with correct keywords.

## Minimal working example

Here is a minimal piece of LaTeX code that should work with a TeX file generated using `rodinapi-tex` (assuming the generated file is called `file.tex`):

```latex
... % Relevent heading matters (documentclass, input encoding, etc.)

\input{rodinlisting.tex} % This references the relevent packages including listing
\lstset{
    mathescape=true,
    escapeinside={(*}{*)}
    ... % Other listing style goes here
}

\begin{document}
\lstinputlisting[language=eventb]{file.tex}
\end{document}

```

# Dependencies

`rodin-tex` only depends on the `rodin-api` package, contained in this repository.

# License

This work is distributed under the GPLv3 license, detailed in the LICENSE file.



