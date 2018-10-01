/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package smartcontracts

import org.scalatest._

import utils._

class SmartContractSolidityOutputSuite extends SmartContractSuite {
  for (args <- validArgs) {
    test(s"stainless --solidity ${args.mkString(" ")}") {
      runMainWithArgs(args :+ "--solidity")
    }
  }

  val invalidFiles = resourceFiles("smartcontracts/failCompilation", _.endsWith(".scala"), false).map(_.getPath).toSeq

  for (file <- invalidFiles) {
    test(s"stainless --solidity $file") {
      // FIXME: make sure the assertion comes from SolidityOutput
      assertThrows[Exception] {
        runMainWithArgs(Array("--solidity", file))
      }
    }
  }
}

class SmartContractSolcSuite extends SmartContractSuite {
  if (solcAvailable) {
    val validSolidityFiles = validFiles.map(_.replaceAll("\\.scala", ".sol"))
    val validSolidityArgs = validSolidityFiles.map(Array(_)) ++ validDirs.map(d => files(d, _.endsWith(".sol")).toArray).filterNot(_.isEmpty)
    for (args <- validSolidityArgs) {
      val cmd = s"solc ${args.mkString(" ")}"
      test(cmd) {
        val (std, exitCode) = runCommand(cmd)
        if (exitCode == 0 && !std.isEmpty)
          println("solc output:\n" + std.mkString("\n"))
        assert(exitCode == 0, "solc failed with output:\n" + std.mkString("\n"))
      }
    }
  } else {
    throw new Exception("You must have `solc` in your path to run the test suite.")
  }
}
