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

  private def shouldAddFile(file: String) = {
    !file.contains("/tmp/")
  }

  def parsePath(path: String) = {
    def rec(l: List[String]):List[String] = l match {
      case Nil => Nil
      case x :: y :: xs if y == ".." => rec(xs)
      case x :: xs => x :: rec(xs) 
    }

    val parsed = rec(path.split("/").filterNot(_.isEmpty).toList)
    parsed.foldLeft("")(_ ++ "/" ++ _)
  }

  def apply(file: String, unit: xt.UnitDef,classes: Seq[xt.ClassDef],functions: Seq[xt.FunDef]): Unit = {
    if(shouldAddFile(file)) {
      files += parsePath(file)
      allClasses ++= classes
      allFunctions ++= functions
    }
  }

  def beginExtractions() = Unit

  // We start the compilation at the end of the extraction to ensure that
  // we have all the dependancies stored in the registry
  final override def endExtractions(): Unit = {
    context.reporter.info("Begin Compilation")
    val symbols = xt.NoSymbols.withClasses(allClasses).withFunctions(allFunctions)
    
    files.foreach{ filename =>
      smartcontract.SolidityCompiler(filename)(symbols, context) 
    }

    context.reporter.info("Compilation Done")
  }

  def failed() = Unit
  def getReport: Option[AbstractReport[_]] = None
  def join() = Unit
  def stop() = Unit

 
}