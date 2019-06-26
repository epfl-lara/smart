package stainless
package smartcontracts

import sys.process._

import java.io.File

package object utils {
  def runCommand(cmd: String): (List[String], Int) = {
    val std = scala.collection.mutable.ListBuffer[String]()
    val exitCode = cmd ! ProcessLogger(std append _)
    (std.toList, exitCode)
  }

  def solcAvailable() = try {
    "solc" ! ProcessLogger(s => Unit)
    true
  } catch {
    case e:Exception => false
  }

  def subdirectories(dir: String): List[String] = {
    val d = new File(dir)
    d.listFiles.toList.filter(_.isDirectory).map(_.getPath)
  }

  def files(dir: String, pred: String => Boolean = _ => true): List[String] = {
    val d = new File(dir)
    d.listFiles.toList.filter((f: File) => f.isFile && pred(f.getPath)).map(_.getPath)
  }

  def runMainWithArgs(args: Array[String]) = {
    val ctx = Main.setup(args).copy(reporter = new inox.TestSilentReporter())
    val compilerArgs = args.toList filterNot { _.startsWith("--") }
    var compiler = frontend.build(ctx, compilerArgs, stainless.Main.factory)
    ctx.reporter.info(s"Running: stainless ${args.mkString(" ")}")
    compiler.run()
    compiler.join()
    compiler.getReport
  }
}