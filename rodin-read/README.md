`rodin-read` -- A transformation utility for exporting Rodin files into LaTeX or
(ASCII) text files.

# Synopsis

`rodin-read` is a program for reading/parsing [Rodin](http://www.event-b.org/install.html)
(XML) files and exporting them to LaTeX or (ASCII) text.

It is also possible to propose _substitution rules_ to automatically transform 
Event-B formulas.

# Options

Basic usage:
```
    rodin-read [-tex|-ascii] <file1> [<file2> [...]] [-d <outdir>] [-s <subst>]
```

Export a list of files (`file1`, `file2`, ...) to the given format and with
the given options.

Options are detailed below. Note that you can access in-program help with
`rodin-read -h`.

## `-tex`

Export the files to LaTeX. This exportation is carried out using the [`rodin-tex`](https://github.com/gdupont1/rodin-hs/tree/main/rodin-tex) package (available in this repository).

File `<file>` is transformed to `<file>.tex`, which can be inputed using
`lstlising`'s `\lstinputlisting` command. Additional files are required, given
in the `rodin-tex/latex` subdirectory.

## `-ascii`

Export the files to text files.

File `<file>` is transformed to `<file>.ascii`. These files are exported using
the writing facilities in [`rodin-api`](https://github.com/gdupont1/rodin-hs/tree/main/rodin-api).

## `-d <outdir>`

Change output directory.

## `-s <subst>`

Specify a substitution file. Note that this option can be specified multiple
times.

Substitution mechanism is described below.


# Substitution Mechanism

A substitution file is a set of rules of the form
```
@<pattern>@ => @<template>@
```

Note that any line that does not contain a `=>` will be ignored (which may be
used to provide comments).

## Patterns

The `<pattern>` part of the rule is specified using a regex-style syntax,
dedicated to Rodin expressions.

A pattern consists of _combinators_ and _token specifications_. Available
combinators are the following:
 * `<pattern>*` arbitrary number of repetitions of `<pattern>`
 * `<pattern>+` one or more repetitions of `<pattern>`
 * `<pattern>?` zero or one occurrences of `<pattern>`
 * `<pattern1>|<pattern2>` either `<pattern1>` or `<pattern2>`

A pattern may also specify _capture groups_ using parentheses (`(<pattern>)`),
that can be used to regroup a sequence of tokens (e.g. for using postfix
combinators as they only refer to the last pattern bit in the chain).

Apart from combinators, a pattern is made of token specifications, which may be
the following:
 * `\o` matches any operator
 * `\I` matches any special identifier (typically, sets)
 * `\s` matches any space (note that, otherwise, spaces are **ignored**)
 * `\k` matches any 'simple token', i.e. delimiters
 * `\i` matches any identifier
 * `.` matches any token
 * Any other stream of characters constitute a token
 * Backslash may be used to escape any special character (combinators, `.` or
 backslash itself)
 * Quotes (`"..."`) may be used to escape a whole sequence and force the parser
 to consider it as one token. In between quotes, every character is escaped.
 This is especially useful for long tokens or tokens that consists of special
 characters (e.g. `|->` can be written `"|->"` instead of the slightly less
 readable `\|->`)

### Special Pattern Reduction Rules

Once parsed, a pattern is _normalised_ and then compiled into a finite state
machine. The normalisation procedure is detailed in `substitution` package page,
but you should know about some normalisation rules that may modify the semantics
of the pattern you write:

#### Double capture is removed

Any level of nesting of capture groups are collapsed into one
(`((...)) => (...)`)

#### Double combinator collapse

Any level of nesting of the same combinator are collapsed into one. For
instance, `...**` is equivalent to `...*`.

### Option-Repetition collapse

When repetition is followed by option (or option is followed by repetition),
then option is removed.

Typically: patterns `a?*` and `a*?` are both equivalent to `a*`.

#### Capture-combinators swap

Capture groups may be used as "normal" groups,
i.e. a way to regroup a sub-pattern in order to feed it to a combinator (e.g.
`(a|b)*`). In that case, parsing establishes a pattern of the form
`Repetition (Capture ...))`.

However, and in particular because it would mess up the capture group reference
system, normalisation swaps the combinator and the capture group, so that the
capture is not affected by the combinator, and only the pattern inside is (e.g.
`Capture (Repetition ...)`).


#### Empty pattern neutrality

These rules are mainly here because of the way patterns are being parsed.

Typically, empty pattern is removed as much as possible from the patterns, using
the fact that it is neutral or idem-potent.

For instance, if we denote by `{}` the empty pattern, then `{}a` = `a{}` = `a`.

`{}` is neutral for every combinators: `{}*` = `{}?` = `{}`.

Also, `({}|{}) = {}` (this may happen when you write `(|)` although it is a bit
weird).


## Templates

Templates are basically formula with "holes". Holes are denoted with `$n` where
`n` is a number between 0 and 9 that references a capture group from the pattern.

Note that `$0` always contains the whole matched token string.

Quotes (`"..."`) may be used to force the template parser to consider a string
as one single token. Note that, between quotes, every character is considered
escaped (except the quote itself of course).

In addition, such an "escaped token" may with `#` to notify that what comes next
should be output _verbatim_. This is especially useful when outputting "complex"
LaTeX code that could be misinterpreted otherwise. For instance, one could want
to use superscript or subscripts in the final file (e.g. `V_{\mathit{min}}^0`).
This can be achieved using `"#..."` (in this case, `"#V_{\mathit{min}}^0"`).

Note that there is currently no way of using variables in an escape sequence
(`"...$..."` will most likely print a dollar sign...).


## Examples

 1. Replace any occurrence of the identifier `my_id` with `id`
```
@my_id@ => @id@
```

 2. Replace any occurrence of the identifier `REAL` with the symbol for real numbers
`\mathbb{R}`
```
@"REAL"@ => @"#\mathbb{R}"@
```

 3. Switch from set notation to infix notation for relations
```
@(\i) "|->" (\i) : R@ => @$1 R $2@
```

## Limitations

As for now the substitution system presents the following limitations:
 * Identifiers are matched as a whole; no pattern for identifiers (e.g. you
 cannot write a pattern that says "given an identifier _starting with_ ...")
 * You cannot splice identifiers in the output (i.e. put capture group
 references within an identifier in the rule's RHS)
 * You cannot specify a range of repetition number (extended-regex style with
 `(...){1,3}` for example)
 * Just like for standard regex, only regular languages are recognised. You
 cannot match "this closing parenthesis matching the _corresponding opening
 one_".


# Extending the tool

It is possible to define custom configurations to be used by the tool, so that
it can read new types of file, and can output to other formats.

Note that there is no way to do so without diving (a little bit) in the code.

## Basic principles

The program spans two directions : _reading_ and _writing_.

Reading is captured by `ReadConfig`, while writing is captured by `WriteConfig`.

Informally, reading is associated to the `Parsable` typeclass and `Parsed` type.
Writing is associated to a number of typeclasses, which have in common they
provide a function `a -> String`.


## Adding a new type of input file

Input formats are managed in `Options` using
 1. The `Parsable` typeclass, used to defined type-generic parser
 2. The `Parsed` type, used to define the list of acceptable parsable types
 3. The `ReadConfig` type, used to define a parsing configuration
 4. The `readConfTable` constant, that contains a table of every parsable types
 together with their respective name and file extension (to link a file with the
 relevant parser).

Adding a new file type is thus done by extending these 4 elements.

### The `Parsable` typeclass and `Parsed` type

Types that may be parsed must be able to be reduced to an object of type
`Parsed`. This is done using a typeclass that simply makes the association
between the type and an element of type `Parsed`, through the use of a
constructor.

For example, if `A` is a type that may be parsed, then it is an instance of
`Parsable`. In addition, `Parsed` must have a particular constructor to wrap
elements of type `A`, for example `PA` (the `Parsable` instance then is that
constructor, i.e. `toParsed = PA`).

We provide an instance of each "writing" typeclass for `Parsed`; consequently,
when you extend the `Parsed` type, you must provide an additional case in each
instance of these classes.

Additionally, you must provide an additional case for your constructor in the
`Substituable` typeclass (meaning your type _must be substituable_ as well).

### The `ReadConfig` type and the `readConfTable` constant

`ReadConfig` is a type that represents a reader, i.e. an object capable of
reading a file and generating a `Parsed` object. A reader is associated to a
name, mainly for messaging purposes.

`ReadConfig`s are gathered in a constant, `readConfTable`, that is an associative
list linking _file extensions_ to reader configurations.

Once you have defined a new type to be read, you must add an entry in
`readConfTable`, i.e. a pair, which left element is the file extension and the
right element is the `ReadConfig` representing the parser for your type.

(You may also add an entry in the help message for the program :0) )

You can use the `mkParser` function to create a viable parsing function from
your parsing function, to be used with `ReadConfig`s.


## Adding a new type of output file

This is a bit more complicated. Output formats are managed in `Options` as well,
and revolves around:
 1. A particular kind of _typeclasses_
 2. The `WriteConfig` type, used to represent a writer/exporter configuration
 3. The `writeConfTable` constant, that contains a table of every type of
 exporter.

### _Exporting-kind_ typeclasses

A first step to define a new exporter is to define your own typeclass. This 
typeclass must define at least one function of signature `a -> String`, which
performs the actual exporting.

Of course, once defined, every defined type that can be parsed must be
associated to an instance for this typeclass. We use typeclasses in particular
to implement a kind of visitor design pattern.

### The `WriteConfig` type and the `wirteConfTable` constant

`WriteConfig` is a type that represents an exporter, i.e. an object capable of
writing a file based on `Parsed` object. It consists of a name for the exporter,
a file extension (for generated files) and the exporting function.

`WriteConfig`s are gathered in a constant, `writeConfTable`, that is an
associative list linking _options_ to writer configurations.

The second step for adding a new exporter is to add an entry in
`writeConfTable`, i.e. a pair which left side is the _option_ to invoke that
writer, and the right side is the writer configuration.

Note that that "option" string is what is used in the options of the program
to use the export. If you link the writer to string `"abc"`, then you can
summon this writer by using option `-abc`.

(You should also add an entry in the help message...)


