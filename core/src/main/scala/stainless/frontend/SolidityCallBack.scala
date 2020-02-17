package stainless
package frontend

import extraction.xlang.{trees => xt, TreeSanitizer, SmartContractsSanitizer}
import scala.concurrent.Future
import java.nio.file

import scala.collection.mutable.{ Map => MutableMap, Set => MutableSet, ListBuffer}

import scala.language.existentials

import stainless.utils.LibraryFilter

class SolidityCallBack(implicit val context: inox.Context)
  extends CallBack with StainlessReports { self =>
  import context.reporter

  val files = MutableSet[String]()
  val allClasses = ListBuffer[xt.ClassDef]()
  val allFunctions = ListBuffer[xt.FunDef]()
  val allTypeDefs = ListBuffer[xt.TypeDef]()

  override def apply(
    file: String,
    unit: xt.UnitDef,
    classes: Seq[xt.ClassDef],
    functions: Seq[xt.FunDef],
    typeDefs: Seq[xt.TypeDef]
  ): Unit = {
    synchronized {
      if (unit.isMain) {
        files += file
      }
      allClasses ++= classes
      allFunctions ++= functions
      allTypeDefs ++= typeDefs
    }
  }

  def beginExtractions() = {}

  // We start the compilation at the end of the extraction to ensure that
  // we have all the dependencies stored in the registry
  final override def endExtractions(): Unit = {
    context.reporter.info("Begin Compilation")
    val allSymbols = xt.NoSymbols
      .withClasses(allClasses)
      .withFunctions(allFunctions)
      .withTypeDefs(allTypeDefs)

    val initialSymbols = LibraryFilter.removeLibraryFlag(allSymbols)

    def notUserFlag(f: xt.Flag) = f.name == "library" || f == xt.Synthetic

    val userIds =
      initialSymbols.classes.values.filterNot(cd => cd.flags.exists(notUserFlag)).map(_.id) ++
      initialSymbols.functions.values.filterNot(fd => fd.flags.exists(notUserFlag)).map(_.id) ++
      initialSymbols.typeDefs.values.filterNot(td => td.flags.exists(notUserFlag)).map(_.id)

    val userDependencies = (userIds.flatMap(initialSymbols.dependencies) ++ userIds).toSeq

    val smartcontracts = context.options.findOptionOrDefault(optSmartContracts)
    val smartcontractsGroup = if (smartcontracts) Some("smart-contracts") else None
    val keepGroups = context.options.findOptionOrDefault(optKeep) ++ smartcontractsGroup

    def hasKeepFlag(flags: Seq[xt.Flag]) =
      keepGroups.exists(g => flags.contains(xt.Annotation("keep", Seq(xt.StringLiteral(g)))))

    def keepDefinition(defn: xt.Definition): Boolean =
      hasKeepFlag(defn.flags) || userDependencies.contains(defn.id)

    val preSymbols =
      xt.NoSymbols.withClasses(initialSymbols.classes.values.filter(keepDefinition).toSeq)
                  .withFunctions(initialSymbols.functions.values.filter(keepDefinition).toSeq)
                  .withTypeDefs(initialSymbols.typeDefs.values.filter(keepDefinition).toSeq)

    val symbols = Recovery.recover(preSymbols)

    val errors = TreeSanitizer(xt).enforce(symbols)
    if (!errors.isEmpty) {
      reportErrorFooter(symbols)
    }

    symbols.ensureWellFormed
    TreeSanitizer(xt).check(symbols)
    SmartContractsSanitizer(xt).check(symbols)

    files.foreach { file =>
      solidity.SolidityOutput(file)(symbols, context)
    }

    context.reporter.info("Compilation Done")
  }

  def failed() = {}

  def stop(): Unit = {}

  def join(): Unit = {}

  def getReport: Option[AbstractReport[_]] = {
    if (!files.isEmpty) Some(new NoReport())
    else None
  }

  private def reportError(pos: inox.utils.Position, msg: String, syms: xt.Symbols): Unit = {
    reporter.error(pos, msg)
    reportErrorFooter(syms)
  }

  private def reportErrorFooter(syms: xt.Symbols): Unit = {
    reporter.error(s"The extracted program is not well formed.")
    reporter.error(s"Symbols are:")
    reporter.error(s"functions -> [${syms.functions.keySet.toSeq.sorted mkString ", "}]")
    reporter.error(s"classes   -> [\n  ${syms.classes.values mkString "\n  "}\n]")
    reporter.error(s"typedefs  -> [\n  ${syms.typeDefs.values mkString "\n  "}\n]")
    reporter.fatalError(s"Aborting from BatchedCallBack")
  }
}