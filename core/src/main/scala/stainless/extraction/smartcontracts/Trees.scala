/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction
package smartcontracts

trait Trees extends methods.Trees { self =>

  def isThis(e: Expr) = e match {
    case This(_) => true
    case _ => false
  }

  case object Payable extends Flag("solidityPayable", Seq.empty)

  override def extractFlag(name: String, args: Seq[Expr]): Flag = (name, args) match {
    case ("solidityPayable", Seq()) => Payable
    case _ => super.extractFlag(name, args)
  }

  override def getDeconstructor(that: inox.ast.Trees): inox.ast.TreeDeconstructor { val s: self.type; val t: that.type } = that match {
    case tree: Trees => new TreeDeconstructor {
      protected val s: self.type = self
      protected val t: tree.type = tree
    }.asInstanceOf[TreeDeconstructor { val s: self.type; val t: that.type }]

    case _ => super.getDeconstructor(that)
  }

  implicit class SmartContractsFunDefWrapper(fd: FunDef) {
    def isPayable: Boolean = fd.flags.contains(Payable)
    def isInSmartContract(implicit symbols: self.Symbols): Boolean = {
      fd.flags.exists {
        case IsMethodOf(cid) =>
          val cd = symbols.getClass(cid)
          cd.isContract

        case _ => false
      }
    }

    def isConstructor(implicit symbols: self.Symbols): Boolean = fd.isContractMethod && fd.id.name == "constructor"

    def isInvariant(implicit symbols: self.Symbols): Boolean = fd.isInSmartContract && (fd.id.name == "invariant" || fd.id.name == "contractInvariant")
    def isContractMethod(implicit symbols: self.Symbols): Boolean = !fd.isInvariant && fd.isInSmartContract && !fd.isAccessor
  }

  implicit class SmartContractsClassDefWrapper(cd: ClassDef) {
    def isContract: Boolean = {
      !isIdentifier(contractID, cd.id) &&
      !isIdentifier(contractInterfaceID, cd.id) &&
      cd.parents.exists { acd =>
        isIdentifier(contractID, acd.id) ||
        isIdentifier(contractInterfaceID, acd.id)
      }
    }
  }

  type Symbols >: Null <: AbstractSymbols

  trait AbstractSymbols
    extends super.AbstractSymbols
       with DependencyGraph { self0: Symbols =>
  }
}

trait Printer extends methods.Printer {
  protected val trees: Trees
}

trait TreeDeconstructor extends methods.TreeDeconstructor {
  protected val s: Trees
  protected val t: Trees

  override def deconstruct(f: s.Flag): DeconstructedFlag = f match {
    case s.Payable => (Seq(), Seq(), Seq(), (_, _, _) => t.Payable)
    case _ => super.deconstruct(f)
  }
}
