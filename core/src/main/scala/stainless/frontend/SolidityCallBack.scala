package stainless
package frontend

import extraction.xlang.{ trees => xt }
import scala.concurrent.Future
import java.nio.file

import scala.collection.mutable.{ Map => MutableMap, Set => MutableSet, ListBuffer}

class SolidityCallBack(implicit val context: inox.Context) 
  extends CallBack { self =>
  val trees = xt
  val files = MutableSet[String]()
  val allClasses = ListBuffer[xt.ClassDef]()
  val allFunctions = ListBuffer[xt.FunDef]()

  def apply(file: String, unit: xt.UnitDef, classes: Seq[xt.ClassDef], functions: Seq[xt.FunDef]): Unit = {
    if (unit.isMain) {
      files += file
    }
    allClasses ++= classes
    allFunctions ++= functions
  }

  def beginExtractions() = {}

  // We start the compilation at the end of the extraction to ensure that
  // we have all the dependancies stored in the registry
  final override def endExtractions(): Unit = {
    context.reporter.info("Begin Compilation")
    val symbols = xt.NoSymbols.withClasses(allClasses).withFunctions(allFunctions)

    symbols.ensureWellFormed
    
    files.foreach{ file =>
      soliditycompiler.SolidityCompiler(file)(symbols, context) 
    }

    context.reporter.info("Compilation Done")
  }

  def failed() = {}
  def getReport: Option[AbstractReport[_]] = None
  def join() = {}
  def stop() = {}
}