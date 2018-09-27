.. smartcontracts:

Stainless Smart Contracts
=========================

Overview
--------

Stainless now has experimental support for formally verifying smart contracts,
and compiling them to Solidity.

Installation
------------

Development happens on the `smart repository
<https://github.com/epfl-lara/smart>`_, a fork of `Stainless
<https://github.com/epfl-lara/stainess>`_ which is frequently rebased on top of
Stainless's master. You can follow the Stainless `installation guide
<installation.rst>`_ by changing the repository:

.. code-block:: bash

  $ git clone https://github.com/epfl-lara/smart.git
  Cloning into 'smart'...
  // ...
  $ cd smart
  $ sbt clean universal:stage
  // takes about 1 minute

You can then create a symbolic link (e.g. for Linux & Mac OS-X) to have access 
to a ``stainless`` command-line (assuming ~/bin is in your path).

.. code-block:: bash

  $ ln -s /path/to/smart/frontends/scalac/target/universal/stage/bin/stainless-scalac ~/bin/stainless


Formal Verification of Smart Contracts
--------------------------------------

Let us have a look at ``MinimumToken`` which implements a token with just a
``transfer`` function. From the ``smart`` repository, issue the following
commands:

.. code-block:: bash 

  $ cd frontends/benchmarks/smartcontracts/valid/MinimumToken
  $ stainless *.scala

After 30 seconds or so, Stainless should report that all verification conditions
are valid. What do these correspond to? The file ``MinimumToken.scala`` defines
a token with a ``transferFrom`` function. 

.. code-block:: scala

  def transferFrom(from: Address, to: Address, amount: Uint256): Unit = {
    require(contractInvariant(this))

    // input validation at runtime
    dynRequire(to != Address(0))
    dynRequire(from != to)
    dynRequire(amount <= balanceOf(from))

    // ghost code to update the list of participants
    ghost {
      addParticipant(from)
      addParticipant(to)
    }

    // balanceOf mapping before any update
    @ghost val b0 = Mapping.duplicate(balanceOf)

    // code to remove balance from `from` address
    balanceOf set (from, balanceOf(from) - amount)

    // balanceOf mapping before after the first update, before the second update
    @ghost val b1 = Mapping.duplicate(balanceOf)

    // code to add balance to recipient `to`
    balanceOf set (to, balanceOf(to) + amount)

    // proof that the sum of balances stays equal to `total`
    assert((
      sumBalances(participants, balanceOf)                                    ==| balancesUpdatedLemma(participants, b1, to, b1(to) + amount) | 
      sumBalances(participants, b1) - b1(to) + (b1(to) + amount)              ==| trivial |
      sumBalances(participants, b1) + amount                                  ==| (balancesUpdatedLemma(participants, b0, from, b0(from) - amount) && sumBalances(participants, b1) == sumBalances(participants, b0) - b0(from) + (b0(from) - amount)) |
      sumBalances(participants, b0) - b0(from) + (b0(from) - amount) + amount ==| ((b0(from) - amount) + amount == b0(from)) |
      sumBalances(participants, b0) - b0(from) + b0(from)                     ==| trivial |
      sumBalances(participants, b0)                                           ==| trivial |
      total
    ).qed)

  } ensuring { _ =>
    contractInvariant(this)
  }

Ignoring the body of the function for a while, the ``require`` and ``ensuring``
annotations (pre and post-conditions) ask Stainless to show that, regardless
with which arguments the ``transferFrom`` function is called, as long as the
contract `invariant` holds before the function call, then it will still hold
after the function call. 

The ``contractInvariant`` function is defined in the file
``MinimumTokenInvariant.scala``.

.. code-block:: scala

  def contractInvariant(contract: MinimumToken): Boolean = {
    distinctAddresses(contract.participants) && 
    sumBalances(contract.participants, contract.balanceOf) == contract.total &&
    forall((x: Address) => 
      (contract.balanceOf(x) != Uint256.ZERO) ==> 
      contract.participants.contains(x)
    )
  }

It states that all addresses that appear in the (ghost) variable participants
are distinct, that the sum of all balances of participants equals to 
`total`, and that all addresses with a non-zero balance appear in the list of 
participants. 

Showing that this invariant holds after the updates that happens in the
`transferFrom` function requires some work. Some lemmas that are used to relate
the sum of all balances before and after updates are stated and proven in the
``MinimumTokenInvariant.scala`` file. In the `transferFrom` function, we then
invoke the lemmas using assertions that will be used for verification. These
`ghost` expressions are ignored during compilation. 

The ``==|`` and ``|`` notations are defined in ``stainless.equations``. They
enable to prove that two expressions are equal by detailing the sequence of
intermediary steps, while providing evidence for each step (or ``trivial`` if
not evidence is required).

`MinimumToken` is not so useful as is, since there is no way to create tokens.
As an exercise, the reader may try to add a function for minting tokens, and
prove that this function maintains `contractInvariant`. Additionally, we can add
a custom constructor to this contract by adding a function called `constructor`
which will be translated to a constructor in Solidity during compilation.


Compilation to Solidity
-----------------------

The ``MinimumToken`` example can be compiled to Solidity using the following
command (still in the ``MinimumToken`` folder):

.. code-block:: bash

  $ stainless --solidity *.scala

It will produce the following Solidity code (in the file `MinimumToken.sol`),
which can be compiled by the Solidity compiler to Ethereum Virtual Machine
bytecode.

.. code-block:: javascript

    function transferFrom (address from, address to, uint256 amount) public {
        require(!(to == address(0)), "error");
        require(!(from == to), "error");
        require(amount <= balanceOf[from], "error");
        balanceOf[to] = balanceOf[to] + amount;
        balanceOf[from] = balanceOf[from] - amount;
    }

All ghost expressions have been eliminated, and only the dynamic requires 
(``dynRequire``) and the code that updates the balances remain.


Features
--------


Ghost code
^^^^^^^^^^

Ghost code which is annotated with the ``@ghost`` annotation is ignored when
compiling the smart contracts to Solidity.

Static and Dynamic Checks
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Importing ``stainless.lang.StaticChecks._`` provides the keywords ``assert`` and
``require`` which trigger the creation of verification conditions. These
expressions are ghost will not be compiled to Solidity, which allows you to 
save on gas cost once your contracts are deployed.

On the other hand, importing ``stainless.smartcontracts._`` gives you the
keywords ``dynAssert`` and ``dynRequire`` which do not trigger the creation of
verification conditions, and which *do* get compiled to Solidity (respectively
to ``assert`` and ``require``) to get runtime checks.

**Remark: Beware if you use require on external functions, as these will not
appear in the compiled Solidity code. If you want both static verification
conditions (from internal calls) and dynamic checks at runtime, you can use both
require and dynRequire as follows.**


.. code-block:: scala

  def f() = {
    require(condition)
    dynRequire(condition)
    // rest of the code
  }


Strict Arithmetic
^^^^^^^^^^^^^^^^^

The ``--strict-arithmetic`` mode makes Stainless add verification conditions
(VCs) that check that arithmetic operations do not overflow. For instance, when
the mode is active, writing ``a + b`` if ``a`` and ``b`` are ``uint256̀`` will
create a VC stating that ``a + b`` must be greater or equal to ``a``, and
Stainless will report whether this VC is valid or not (or unknown). 


Development
-----------

The `smart repository <https://github.com/epfl-lara/smart>`_ is in active
development and you should expect many (possibly backward-incompatible) changes
as we implement new features. Here is a list of things that we are working on,
or plan to work on in the near future:

* Conversion from Address to Contract.
* Direct compilation to EVM bytecode and other backends.
* Inheritance between contracts (case classes will be replaced by traits).
* Fallback functions.
* More uintX types (only uint8 and uint256 are supported for the moment).
* @internal and @external annotations for functions, to denote functions that 
  can only be accessed from the inside or outside, respectively.
* For loops (at the moment, while loops or recursive functions can be used instead).


If you would love to a see a feature which is not listed here, please open an
issue in the `smart repository <https://github.com/epfl-lara/smart>`_.


Known Issues
------------

* Your code must contain a case class that extends the `Contract` class (from stainless.smartcontracts), otherwise you will get an exception during verification.
* For readability, the compiler to Solidity currently print the names of the variables as they appear in your Stainless source code. As such, you should avoid using two variables with the same name in the same scope.

Reporting Issues
----------------

As you start experimenting with your own smart contracts in Stainless, you may
encounter bugs with verification and compilation. These can be reported in the
`smart repository <https://github.com/epfl-lara/smart>`_. You may also
get help in the `gitter channel <https://gitter.im/epfl-lara/smart>`_.