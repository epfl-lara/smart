Stainless 0.1 [![Build Status](http://laraquad4.epfl.ch:9000/epfl-lara/stainless/status/master)](http://laraquad4.epfl.ch:9000/epfl-lara/stainless) [![Gitter chat](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/epfl-lara/stainless)
=============

Verification framework for a subset of the [Scala](http://scala-lang.org) programming language.
Supports contract-driven verification as well as termination checking of higher-order
functional programs with local imperative features (see [Pure Scala](core/src/sphinx/purescala.rst)
and [Imperative](core/src/sphinx/imperative.rst)
for more details about the supported fragment).

To get started, see the documentation chapters, such as
  * [Installation](core/src/sphinx/installation.rst)
  * [Getting Started](core/src/sphinx/gettingstarted.rst)
  * [Introduction to Stainless](core/src/sphinx/intro.rst)

### Smart Contracts

Stainless supports verification of [smart
contracts](core/src/sphinx/smartcontracts.rst) written in (a subset of) Scala.
In addition to verifying invariant and assertions of your smart contracts,
Stainless can compile them to Solidity.

#### Disclaimer

Support for smart contracts is highly experimental. This branch is in active
development and subject to frequent changes. Only use it with great caution, 
and at your own risk.


### Relation to [Inox](https://github.com/epfl-lara/inox)

Stainless relies on Inox to solve the various queries stemming from program verification.
Inox supports model-complete queries in a feature-rich fragment that lets Stainless focus
on program transformations and soundness of both contract and termination checking.

### Relation to [Leon](https://github.com/epfl-lara/leon)

The Stainless/Inox stack has grown out of the Leon codebase and subsumes the verification and
termination checking features of Leon. The new projects aim to provide a more stable and
principled implementation of the verification techniques underlying Leon. Feature-wise,
Stainless has already outgrown Leon verification and provides new features such as higher-order
contracts and contract-based termination checking.
