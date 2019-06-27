/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package smartcontracts

import org.scalatest._

import utils._

class SmartContractSolidityOutputSuite extends SmartContractSuite {
  for (args <- validArgs if !args.isEmpty) {
    val solcCompile = s"solc ${args.mkString(" ")}".replaceAll("\\.scala", ".sol")
    test(s"stainless --solidity --overwrite-sol ${args.mkString(" ")}; $solcCompile") {
      runMainWithArgs(args :+ "--solidity" :+ "--overwrite-sol")
      val (std, exitCode) = runCommand(solcCompile)
      if (exitCode == 0 && std.mkString != "Compiler run successful, no output requested.")
        println("solc output:\n" + std.mkString("\n"))
      assert(exitCode == 0, "solc failed with output:\n" + std.mkString("\n"))
    }
  }

  // val invalidFiles = resourceFiles("smartcontracts/failCompilation", _.endsWith(".scala"), false).map(_.getPath).toSeq

  // for (file <- invalidFiles) {
  //   test(s"stainless --solidity $file") {
  //     // FIXME: make sure the assertion comes from SolidityOutput
  //     assertThrows[Exception] {
  //       runMainWithArgs(Array("--solidity", file))
  //     }
  //   }
  // }
}
