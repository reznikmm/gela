Object Ada Semantic Interface Specification _(oasis)_
=========================================

[![reuse compliant](https://img.shields.io/badge/reuse-compliant-green.svg)](https://reuse.software/)

This project is a development of [ASIS](https://www.sigada.org/WG/asiswg/) rewritten in
[OOP](https://en.wikipedia.org/wiki/Object-oriented_programming) style using Ada 2012
features. An application uses this interface to get semantic, syntactic and lexical
information about an Ada program or a library.

Right now we don't have any implementation, just API specification.

## Usage
Add `with "oasis";` to your project file.

### Abstract syntax tree elements
Here is [concise description](docs/ast.md) of AST.

## Related works

 * [Ada ANTLR grammars[(https://github.com/okellogg/ada_antlr_grammar)
 * [WisiToken](http://stephe-leake.org/ada/wisitoken.html)

## Maintainer

[@MaximReznik](https://github.com/reznikmm).

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/reznikmm/oasis/issues/new)
or submit PRs.

## License

[MIT](LICENSE) © Maxim Reznik

