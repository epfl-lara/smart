# Stainless for Smart Contracts

# [![Release][release-img]][latest-release] [![Build Status][larabot-img]][larabot-ref] [![Gitter chat][gitter-img]][gitter-ref] [![Apache 2.0 License][license-img]][license-ref]

Stainless for Smart Contracts 0.7.4s is a fork of [Stainless](https://github.com/epfl-lara/stainless)
which showcases formal verification of smart contracts written in a subset of
Scala, and the compiler of this subset to Solidity.

# Installation

```bash
$ git clone https://github.com/epfl-lara/smart.git
$ cd smart
$ sbt clean universal:stage
```

You can then create a symbolic link (e.g. for Linux & Mac OS-X) to have access
to a ``stainless`` command-line.

```bash
 ln -s frontends/scalac/target/universal/stage/bin/stainless-scalac stainless
```

Fore more information, you can refer to the Stainless documentation:
  * [Stainless Smart Contracts](core/src/sphinx/smartcontracts.rst)
  * [More Detailed Installation Instructions for Stainless](core/src/sphinx/installation.rst)

# Formal Verification

To get the flavor of verification of smart contracts, consult
[the examples in the repository](frontends/benchmarks/smartcontracts/valid).

[`Candy`](frontends/benchmarks/smartcontracts/valid/Candy.scala)
is a simple smart contract written in our language. The
constructor of the contract takes an initial number of candies, which can then
be eaten by the `eatCandy` function. The contract maintains the
invariant that the sum of eaten and remaining candies equals the initial candies.

```scala
import stainless.smartcontracts._
import stainless.lang.StaticChecks._
import stainless.annotation._

trait Candy extends Contract {
  var initialCandies: Uint256
  var remainingCandies: Uint256
  var eatenCandies: Uint256

  @solidityPublic
  final def constructor(_candies: Uint256) = {
    initialCandies = _candies
    remainingCandies = _candies
    eatenCandies = Uint256.ZERO
  }

  @solidityPublic
  final def eatCandy(candies: Uint256) = {
    dynRequire(candies <= remainingCandies)

    remainingCandies -= candies
    eatenCandies += candies
  }

  @ghost @inline
  final def invariant(): Boolean = {
    eatenCandies <= initialCandies &&
    remainingCandies <= initialCandies &&
    initialCandies - eatenCandies == remainingCandies
  }
}
```

Stainless is able to verify that the `constructor` creates a state where the
invariant holds, and the `eatCandy` function modifies the state in a way that
preserves the invariant. Verification for `Uint256` examples is faster if you
configure stainless to use the external [CVC4](http://cvc4.cs.stanford.edu/web/)
solver (which needs to be downloaded separately and added to your path):

> ./stainless frontends/benchmarks/smartcontracts/valid/Candy.scala --solvers=smt-cvc4

```
[  Info  ]   ┌───────────────────┐
[  Info  ] ╔═╡ stainless summary ╞═════════════════════════╗
[  Info  ] ║ └───────────────────┘                         ║
[  Info  ] ║ constructor  body assertion  valid ... 0.122  ║
[  Info  ] ║ eatCandy     body assertion  valid ... 11.109 ║
[  Info  ] ╟┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄╢
[  Info  ] ║ total: 2    valid: 2 ... invalid: 0  ...      ║
[  Info  ] ╚═══════════════════════════════════════════════╝
```

# Compilation to Solidity

The contract can be compiled to Solidity using

> stainless frontends/benchmarks/smartcontracts/valid/Candy.scala --solidity

which produces a file `Candy.sol`. The compiler drops the assertions, but
compiles the `dynRequire` (dynamic require) commands to `require` in Solidity.
The compiler also drops the function `invariant` which is marked with `@ghost`.

```solidity
pragma solidity ^0.7.2;

contract Candy {
    // Fields
    uint256 initialCandies;
    uint256 remainingCandies;
    uint256 eatenCandies;

    // Constructor
    constructor (uint256 _candies) public {
        initialCandies = _candies;
        remainingCandies = _candies;
        eatenCandies = 0;
    }

    // Public functions
    function eatCandy (uint256 candies) public {
        require(candies <= remainingCandies, "error");
        remainingCandies = remainingCandies - candies;
        eatenCandies = eatenCandies + candies;
    }

    // Private functions
    function invariant () view private returns (bool) {
        return eatenCandies <= initialCandies && remainingCandies <= initialCandies &&
          initialCandies - eatenCandies == remainingCandies;
    }
}
```

# Disclaimer

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.


See also the [license](LICENSE).

[latest-release]: https://github.com/epfl-lara/smart/releases/latest
[license-img]: https://img.shields.io/badge/license-Apache_2.0-blue.svg?color=134EA2
[license-ref]: https://github.com/epfl-lara/smart/blob/master/LICENSE
[gitter-img]: https://img.shields.io/gitter/room/gitterHQ/gitter.svg?color=ed1965
[gitter-ref]: https://gitter.im/epfl-lara/smart
[larabot-img]: http://laraquad4.epfl.ch:9000/epfl-lara/smart/status/master
[larabot-ref]: http://laraquad4.epfl.ch:9000/epfl-lara/smart/builds
[release-img]: https://img.shields.io/github/release-pre/epfl-lara/stainless.svg
[tag-date-img]: https://img.shields.io/github/release-date-pre/epfl-lara/stainless.svg?style=popout
