/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package smartcontracts

import utils._

import org.scalatest._

class SmartContractVerificationSuite extends SmartContractSuite {
  val defaultArgs = Array("--smart-contracts", "--solvers=smt-z3,smt-cvc4", "--vc-cache=false")

  for (args <- validArgs) {
    test("stainless " + args.mkString(" ")) {
      val report = runMainWithArgs(defaultArgs ++ args)
      assert(report.get.stats.invalid == 0)
      assert(report.get.stats.unknown == 0)
    }
  }

  val invalidFiles = resourceFiles("smartcontracts/failVerification", _.endsWith(".scala"), false).map(_.getPath).toSeq

  for (file <- invalidFiles) {
    test(s"stainless $file") {
      val report = runMainWithArgs(defaultArgs :+ file)
      assert(report.get.stats.invalid > 0)
    }
  }
}
