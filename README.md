# texhs: Parsing LaTeX in Haskell

`texhs` is a Haskell library for parsing TeX and LaTeX documents
with support for category code management, user-defined commands,
cross-references and BibLaTeX.

It ships with an executable that can be used to convert conforming
LaTeX documents to (TEI-)XML, HTML or EPUB.

In addition, `texhs` provides access to the structure
of an input document at three layers:

- **Token Stream**:
  The lexing module provides a token stream
  after expansion of user-defined macros and
  selected TeX primitives that need
  to be interpreted during lexing
  (e.g. catcode assignments, macro definitions, conditionals).
- **Syntax Tree**:
  The TeX parsing module provides an abstract syntax tree
  based on the lexer output, capturing the grouping structure
  and macro calls with their arguments.
- **Document Model**:
  The Doc parsing module provides an internal document model
  as an abstract syntax tree
  after expansion and evaluation of all remaining macros.

## Development Status

`texhs` is in active development and can already be used to produce
(TEI-based) XML, HTML (XHTML5, XHTML1) and EPUB versions of conforming
documents. However, there is no official release yet and
at this stage the list of supported TeX internals and LaTeX commands
is still somewhat limited, thus `texhs` will fail on many valid LaTeX
documents. In addition, the default Doc parser enforces a strict
document model by design. This helps to guarantee quality output.

An example document that has been converted using `texhs` from LaTeX to
[XML](http://langsci.github.io/XMLBooks/cfls/1/enfield.xml),
[HTML](http://langsci.github.io/XMLBooks/cfls/1/enfield.html), and
[EPUB](http://langsci.github.io/XMLBooks/cfls/1/enfield.epub)
is the linguistic monograph
*Natural Causes of Language* by N.J. Enfield,
published by [Language Science Press](http://langsci-press.org/catalog/book/48).

## Setup Instructions

`texhs` can be installed using [Cabal](https://www.haskell.org/cabal/)
from local sources. There is no package on hackage yet.

Play with the executable in a cabal sandbox:

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal configure
$ cabal build
$ ./dist/build/texhs/texhs -h
```

Install the library and the executable:

```
$ cabal install --only-dependencies
$ cabal configure
$ cabal install
```

Run the test suite:

```
$ cabal install --only-dependencies --enable-tests
$ cabal configure --enable-tests
$ cabal build
$ cabal test
```

Generate API documentation:

```
$ cabal haddock
```

## Architecture

TeX is a highly dynamic language in which lexing, parsing, expansion
and evaluation are deeply intertwined. In fact, TeX cannot be parsed
in general (see
[Erdweg and Ostermann 2011](http://dx.doi.org/10.1007/978-3-642-19440-5_26)).

For this reason, `texhs` uses a couple of lexing and parsing heuristics
to fake a more conventional compiler pipeline with clean APIs.
In particular, TeX macros are partitioned into two disjoint sets:

- **Lexer-level commands**
  need to be evaluated during lexing
  because they may change the behavior of the lexer.
  Examples include
  category code changes (`catcode`),
  modifications of the expansion order (`expandafter`, `futurelet`),
  user-level macro definitions (`let`, `def`, `newcommand`) and
  conditionals (`if`, `ifx`).
- **Document-level commands**
  need not be evaluated during lexing or parsing.
  However, the parameter specification has to be known
  during parsing in order to determine their arguments.
  Examples include
  formatting instructions (`emph`, `textit`),
  sectioning commands (`section`, `chapter`),
  anchors and cross-references (`label`, `ref`) and
  bibliographic references (`cite`).

The resulting pipeline has two main parts. The first part parses a TeX
source document, evaluating lexer-level commands, into an abstract
syntax tree:

```
TeX Source --(tex-lexer)--> Tokens --(tex-parser)--> TeX AST
```

The lexer is currently implemented as a monad transformer on top of
[parsec](http://hackage.haskell.org/package/parsec).
The underlying monad has to instantiate the `HandleTeXIO` class
in order to support certain IO-related functions that handle
side-effecting TeX macros that have to be evaluated during lexing
(in particular, reading files from disk using `input` or `include`
and accessing the current date using `year` and related commands).
Two instances for the type class are provided, `IO` (execute)
and `Identity` (ignore).

Note that it is impossible in general to preprocess `input` commands
in TeX documents by scanning for `\input` strings. For example, the `input`
primitive may be disguised as an arbitrary active character whose
meaning can only be identified by evaluating earlier parts of the TeX source.

The TeX parser builds an abstract syntax tree based on the token
stream produced by the lexer. Since all effectful macros are already
handled during lexing, the parser can be implemented as a pure
function on top of parsec.

The second part of the pipeline interprets the TeX tree and constructs
a document representation that can then be converted to output formats
like XML, HTML and EPUB:

```
TeX AST --(doc-parser)--> Document Model --(writers)--> XML
```

Both the TeX parser and the Doc parser require information about
supported TeX commands. The TeX parser needs to know their syntax
(number and structure of arguments) and the Doc parser needs to
know their semantics in order to map them to appropriate elements
in the document model. The relevant information is stored in a
manually curated TeX command database.
