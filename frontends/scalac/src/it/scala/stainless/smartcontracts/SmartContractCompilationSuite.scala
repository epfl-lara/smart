/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package smartcontract

import sys.process._
import inox.utils.ASCIIHelpers._
import org.scalatest._
import java.io._

class SmartContractCompilationSuite extends FunSuite with inox.ResourceUtils with InputUtils {
  // List of file that have to be discarded by the compiler
  def isDiscardedFile(path: String) = path match {
    case s if s contains "Voting/VotingTokenLemmas.scala" => true
    case s if s contains "Voting/VotingTokenInvariant.scala" => true
    case s if s contains "Voting/Util.scala" => true
    case s if s contains "LoanContract/LoanContractInvariant.scala" => true
    case s if s contains "LoanContract/ERC20Specs.scala" => true
    case s if s contains "MinimumToken/MinimumTokenInvariant.scala" => true
    case _ => false
  }

  val context = stainless.TestContext.empty

  val validFiles = resourceFiles("smartcontracts/valid", _.endsWith(".scala"), true).map(_.getPath).toSeq
  val invalidFiles = resourceFiles("smartcontracts/failCompilation", _.endsWith(".scala"), true).map(_.getPath).toList

  val (_, program1) = loadFiles(invalidFiles)(context)

  invalidFiles.foreach{ path =>
    test(path) {
      assertThrows[Exception] {
        SolidityCompiler(path)(program1.symbols, context)
      }
    }
  }

  val (_, program2) = loadFiles(validFiles)(context)

  val solcAvailable = try {
    "solc" ! ProcessLogger(s => Unit)
    true
  } catch {
    case e:Exception => false
  }

  validFiles.foreach{ path =>
    test("stainless -> solidity " + path) {
      SolidityCompiler(path)(program2.symbols, context)
    }
  }

  validFiles.foreach{ path =>
    if(solcAvailable && !isDiscardedFile(path)) {
      test("solidity -> bytecode " + path) {
        val std = scala.collection.mutable.ListBuffer[String]()  
        val exitCode = ("solc " + path.replace(".scala", ".sol")) ! ProcessLogger(std append _)
        if (exitCode == 0 && !std.isEmpty)
          println("solc output:\n" + std.mkString("\n"))
        assert(exitCode == 0, "solc failed with output:\n" + std.mkString("\n"))
      }
    } else if(!solcAvailable) {
      ignore("solidity -> bytecode " + path) { }
    }
  }
}


