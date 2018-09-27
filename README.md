Stainless 0.1s [![Gitter chat](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/epfl-lara/smart)
=============

Stainless 0.1s is a fork of [Stainless](https://github.com/epfl-lara/stainless) 
which showcases formal verification of smart contracts written in a subset of 
Scala, and their compilation to Solidity. 

# Disclaimer

Support for smart contracts is highly experimental. This fork is in active
development and subject to frequent changes. Only use it with great caution and 
at your own risk.

# Installation


```bash
$ git clone https://github.com/epfl-lara/smart.git
Cloning into 'smart'...
// ...
$ cd smart
$ sbt clean universal:stage
// takes about 1 minute
```

You can then create a symbolic link (e.g. for Linux & Mac OS-X) to have access 
to a ``stainless`` command-line (assuming ~/bin is in your path).

```bash
 ln -s /path/to/smart/frontends/scalac/target/universal/stage/bin/stainless-scalac ~/bin/stainless
```

Fore more information, you can refer to the Stainless documentation:
  * [Installation](core/src/sphinx/installation.rst)
  * [Getting Started](core/src/sphinx/gettingstarted.rst)
  * [Introduction to Stainless](core/src/sphinx/intro.rst)
  * [Stainless Smart Contracts](core/src/sphinx/smartcontracts.rst)


# Formal Verification

`CandyContract` is a simple smart contract written in our language. The
constructor of the contract takes an initial number of candies, which can then 
be eaten by the `eatCandy` function. The contract maintains the
invariant:

> eatenCandies + remainingCandies == initialCandies

```scala
import stainless.smartcontracts._
import stainless.lang.StaticChecks._
import Candy._

case class CandyContract(
  var initialCandies: Uint256,
  var remainingCandies: Uint256,
  var eatenCandies: Uint256
) extends Contract {

  def constructor(_candies: Uint256) = {
    initialCandies = _candies
    remainingCandies = _candies
    eatenCandies = Uint256.ZERO

    assert(invariant(this))
  }

  def eatCandy(candies: Uint256) = {
    require(invariant(this))

    remainingCandies -= candies
    eatenCandies += candies

    assert(invariant(this))
  }
}

object Candy {
  def invariant(c: CandyContract): Boolean = {
    c.eatenCandies + c.remainingCandies == c.initialCandies
  }
}
```

Stainless is able to verify that the assertions written in the contract are 
indeed valid (assuming that the `require` holds).


```
$ stainless Candy.scala
[  Info  ]   ┌───────────────────┐
[  Info  ] ╔═╡ stainless summary ╞══════════════════════════════════════════════════════════════════════╗
[  Info  ] ║ └───────────────────┘                                                                      ║
[  Info  ] ║ constructor      body assertion      valid    U:smt-cvc4     Candy.scala:16:5       0,088  ║
[  Info  ] ║ eatCandy         body assertion      valid    U:smt-cvc4     Candy.scala:26:5       1,766  ║
[  Info  ] ╟┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄╢
[  Info  ] ║ total: 2    valid: 2    (0 from cache) invalid: 0    unknown: 0    time:   1,854           ║
[  Info  ] ╚════════════════════════════════════════════════════════════════════════════════════════════╝
```

Despite the overflows, the invariant property will stay true. Stainless has 
a `--strict-arithmetic` mode that ensures that no overflow happens. Run on this 
example with `--strict-arithmetic`, Stainless produces the following output. 

```
$ stainless Candy.scala --strict-arithmetic
[  Info  ]   ┌───────────────────┐
[  Info  ] ╔═╡ stainless summary ╞══════════════════════════════════════════════════════════════════════╗
[  Info  ] ║ └───────────────────┘                                                                      ║
[  Info  ] ║ constructor   body assertion      valid     U:smt-cvc4   CandyOverflow.scala:16:5    0,060 ║
[  Info  ] ║ eatCandy      integer overflow    invalid   U:smt-cvc4   CandyOverflow.scala:22:5    0,521 ║
[  Info  ] ║ eatCandy      integer overflow    invalid   U:smt-cvc4   CandyOverflow.scala:23:5    0,520 ║
[  Info  ] ║ eatCandy      body assertion      valid     U:smt-cvc4   CandyOverflow.scala:25:5    1,071 ║
[  Info  ] ║ invariant     integer overflow    invalid   U:smt-cvc4   CandyOverflow.scala:31:5    0,315 ║
[  Info  ] ╟┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄╢
[  Info  ] ║ total: 5    valid: 2    (0 from cache) invalid: 3    unknown: 0    time:   2,487           ║
[  Info  ] ╚════════════════════════════════════════════════════════════════════════════════════════════╝

```


These overflows can be fixed by strengthening the invariant to express
the fact that there are no overflows (we need for that an `unsafe_+` operation for which
overflows are ignored), and by requiring (at runtime, using `dynRequire`) that
the number of candies eaten by `eatCandy` is smaller than `remainingCandies`. 
The valid contract is given below, on which Stainless reports 5 valid
verification conditions.

```scala
import stainless.smartcontracts._
import stainless.lang.StaticChecks._
import Candy._

case class CandyContract(
  var initialCandies: Uint256,
  var remainingCandies: Uint256,
  var eatenCandies: Uint256
) extends Contract {

  def constructor(_candies: Uint256) = {
    initialCandies = _candies
    remainingCandies = _candies
    eatenCandies = Uint256.ZERO

    assert(invariant(this))
  }

  def eatCandy(candies: Uint256) = {
    require(invariant(this))
    dynRequire(candies <= remainingCandies)

    remainingCandies -= candies
    eatenCandies += candies

    assert(invariant(this))
  }
}

object Candy {
  def noAdditionOverflow(x: Uint256, y: Uint256) = {
    unsafe_+(x,y) >= x
  }

  def invariant(c: CandyContract): Boolean = {
    noAdditionOverflow(c.eatenCandies, c.remainingCandies) &&
    c.eatenCandies + c.remainingCandies == c.initialCandies
  }
}
```

# Compilation to Solidity

The contract can be compiled to Solidity using 

> stainless --solidity Candy.scala

which produces a file `Candy.sol`:

```javascript
pragma solidity ^0.4.24;

contract CandyContract {
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
}
```
