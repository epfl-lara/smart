/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package xlang

/** Inspect trees, detecting illegal structures. */
trait SmartContractsSanitizer {

  val trees: xlang.Trees
  import trees._

  /** Throw a [[MalformedSmartContract]] exception when detecting an illegal pattern. */
  def check(symbols: Symbols)(implicit ctx: inox.Context): Unit = {
    // TODO: Contract invariants must not make calls to other contracts
    // TODO: Contract variables cannot be accessed outside of the contract

    implicit val sym = symbols
    implicit val printerOpts = PrinterOptions.fromContext(ctx)

    val contracts = symbols.classes.values.filter(_.isContract)
    val allFunctions = symbols.functions.values

    // Contract methods cannot be public and private at the same time
    for (fd <- allFunctions if fd.isSolidityPublic && fd.isSolidityPrivate)
      throw MalformedSmartContract(fd, s"Smart contract methods cannot be both public and private (${fd.id.asString}).")

    // Contract methods must be final
    for (fd <- allFunctions)
      if ((fd.isConcreteContractMethod || fd.isContractInvariant || fd.isContractConstructor) && !fd.isFinal)
        throw MalformedSmartContract(fd, s"Smart contract methods must be final (${fd.id.asString}).")

    // Methods from interfaces must have empty bodies
    for (fd <- allFunctions) {
      if (
        fd.isAbstractContractMethod &&
        !exprOps.deconstructSpecs(fd.fullBody)._2.isEmpty
      )
        throw MalformedSmartContract(fd, s"Methods of contract interfaces must be abstract (${fd.id.asString}).")
    }

    // Methods from concrete contracts must have non-empty bodies
    for (fd <- allFunctions) {
      if (
        fd.isConcreteContractMethod &&
        exprOps.deconstructSpecs(fd.fullBody)._2.isEmpty
      )
        throw MalformedSmartContract(fd, s"Methods of contracts must have an non-empty body (${fd.id.asString}).")
    }

    // Public methods in contracts cannot have preconditions
    for (fd <- allFunctions if fd.isContractMethod && fd.isSolidityPublic) {
      exprOps.preconditionOf(fd.fullBody) match {
        case None =>
        case Some(x) => throw MalformedSmartContract(fd, "Public contract methods cannot have a precondition")
      }
    }

    // Constructors cannot have preconditions
    for (fd <- allFunctions if fd.isContractConstructor) {
      exprOps.preconditionOf(fd.fullBody) match {
        case None =>
        case Some(x) => throw MalformedSmartContract(fd, "Smart contract constructors cannot have a precondition")
      }
    }

    // No inheritance
    for (cd <- contracts if !cd.children.isEmpty)
      throw MalformedSmartContract(cd, s"Inheritance of smart contracts is not supported (${cd.id.asString}).")
  }
}

object SmartContractsSanitizer {
  def apply(tr: xlang.Trees)(implicit ctx: inox.Context): SmartContractsSanitizer { val trees: tr.type } = {
    new SmartContractsSanitizer {
      override val trees: tr.type = tr
    }
  }
}
