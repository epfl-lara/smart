/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package frontend

import stainless.extraction.xlang.{trees => xt}

import scala.util.{Try, Success, Failure}
import scala.concurrent.Await
import scala.concurrent.duration._

class BatchedCallBack(components: Seq[Component])(implicit val context: inox.Context) extends CallBack with StainlessReports {
  private var currentClasses = Seq[xt.ClassDef]()
  private var currentFunctions = Seq[xt.FunDef]()

  private var report: AbstractReport[Report] = _

  protected val pipeline: extraction.StainlessPipeline = extraction.pipeline
  private[this] val runs = components.map(_.run(pipeline))

  def beginExtractions(): Unit = {}

  def apply(file: String, unit: xt.UnitDef, classes: Seq[xt.ClassDef], functions: Seq[xt.FunDef]): Unit = {
    synchronized {
      currentFunctions ++= functions
      currentClasses ++= classes
    }
  }

  def failed(): Unit = {}

  def endExtractions(): Unit = {
    val allSymbols = xt.NoSymbols.withClasses(currentClasses).withFunctions(currentFunctions)
    def notUserFlag(f: xt.Flag) = f.name == "library" || f == xt.Synthetic
    val userIds =
      currentClasses.filterNot(cd => cd.flags.exists(notUserFlag)).map(_.id) ++
      currentFunctions.filterNot(fd => fd.flags.exists(notUserFlag)).map(_.id)
    val userDependencies = userIds.flatMap(id => allSymbols.dependencies(id) ) ++ userIds

    val symbols =
      xt.NoSymbols.withClasses(currentClasses.filter(cd => userDependencies.contains(cd.id)))
                  .withFunctions(currentFunctions.filter(fd => userDependencies.contains(fd.id)))
    val reports = runs map { run =>
      val ids = symbols.functions.keys.toSeq
      val analysis = run(ids, symbols, filterSymbols = true)
      val report = Await.result(analysis, Duration.Inf).toReport
      Some(RunReport(run)(report))
    }
    report = Report(reports collect { case Some(r) => r })
  }

  def stop(): Unit = {
    currentClasses = Seq()
    currentFunctions = Seq()
  }

  def join(): Unit = {}

  def getReport = Option(report)
}
