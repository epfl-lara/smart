import stainless.smartcontracts._
import stainless.lang._
import stainless.annotation._
import stainless.lang._

trait A extends Contract {
    var balance:Uint256

    final def invariant() = {
        balance >= Uint256("120")
    }

    final def increase() = {
        if(balance <= Uint256("250"))
            balance = balance + Uint256.ONE
    }
}

trait B extends Contract {
    var balance:Uint256
    var target:Address

    final def invariant() = {
        balance <= Uint256("120")
    }

    final def decrease() = {
        if(balance > Uint256.ZERO)
            balance = balance - Uint256.ONE
    }

    final def exchange() = {
        require(
            Environment.contractAt(target).isInstanceOf[A]
        )

        Environment.contractAt(target).asInstanceOf[A].increase()
        decrease()

        havoc()
        true
    }
}