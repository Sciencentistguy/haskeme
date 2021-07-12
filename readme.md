# haskeme

A [Scheme](https://schemers.org/Documents/Standards/R5RS/HTML/) interpreter written in Haskell.

This was originally written following the book "Write Yourself a Scheme in 48 Hours".

It differs from the book in many ways. For example:

- It uses [megaparsec](https://github.com/mrkkrp/megaparsec) instead of parsec to parse the input. This is a fork of parsec that produces better error messages and is faster.
- It uses a hashmap to store variables, instead of a list of tuples, making variable access faster.
- It uses modules, rather than one large file enabling more parallel compilation.

---

The book I followed can be found [here](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

`src/Text/Megaparsec/Char/Number.hs` is a port of [this](https://hackage.haskell.org/package/parsec3-numbers-0.1.0) Haskell package (available under the terms of the BSD 3-clause licence) to support [megaparsec](https://github.com/mrkkrp/megaparsec).

The code here is available under the terms of version 3 of the GNU GPL.
