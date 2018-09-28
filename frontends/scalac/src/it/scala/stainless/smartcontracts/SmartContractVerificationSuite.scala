/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package smartcontracts

import utils._

import org.scalatest._

class SmartContractVerificationSuite extends SmartContractSuite {
  val solvers = "--solvers=smt-z3,smt-cvc4"
  for (args <- validArgs) {
    test("stainless " + args.mkString(" ")) {
      val report = runMainWithArgs(solvers +: args)
      assert(report.get.stats.invalid == 0)
      assert(report.get.stats.unknown == 0)
    }
  }

  val invalidFiles = resourceFiles("smartcontracts/failVerification", _.endsWith(".scala"), false).map(_.getPath).toSeq

  for (file <- invalidFiles) {
    test(s"stainless $file") {
      val report = runMainWithArgs(Array(solvers, file))
      assert(report.get.stats.invalid > 0)
    }
  }
}
