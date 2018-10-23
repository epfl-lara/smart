/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package extraction

import scala.language.existentials

package object smartcontracts {
  // Annotation used to represent the keyword payable in solidity
  val payableAnnotation = trees.Annotation("solidityPayable", List())

  object trees extends smartcontracts.Trees with oo.ClassSymbols {
    case class Symbols(
      functions: Map[Identifier, FunDef],
      sorts: Map[Identifier, ADTSort],
      classes: Map[Identifier, ClassDef]
    ) extends ClassSymbols with AbstractSymbols

    object printer extends Printer { val trees: smartcontracts.trees.type = smartcontracts.trees }
  }

  class SmartcontractException(tree: inox.ast.Trees#Tree, msg: String)
    extends MissformedStainlessCode(tree, msg)

  object SmartcontractException {
    def apply(tree: inox.ast.Trees#Tree, msg: String) = new SmartcontractException(tree, msg)
  }

  def extractor(implicit ctx: inox.Context) = {
    val lowering = ExtractionPipeline(new CheckingTransformer {
      override val s: trees.type = trees
      override val t: methods.trees.type = methods.trees
    })

    utils.DebugPipeline("EnvironmentBuilder", EnvironmentBuilder(trees)) andThen
    utils.DebugPipeline("PayDesugaring", PayDesugaring(trees)) andThen
    lowering
  }

  def isSmartContractDep(fd: xlang.trees.FunDef): Boolean = {
    fd.getPos.fullString.contains("stainless/smartcontracts/package.scala") ||
    fd.getPos.fullString.contains("stainless_smartcontracts_package")
  }

  def isSmartContractDep(cd: xlang.trees.ClassDef): Boolean = {
    cd.getPos.fullString.contains("stainless/smartcontracts/package.scala") ||
    cd.getPos.fullString.contains("stainless_smartcontracts_package")
  }

}
