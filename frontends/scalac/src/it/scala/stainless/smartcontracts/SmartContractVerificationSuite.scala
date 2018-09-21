/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package verification

import org.scalatest._

class SmartContractVerificationSuite extends ComponentTestSuite {
  val component = VerificationComponent

  override def filter(ctx: inox.Context, name: String): FilterStatus = name match {
    case "smartcontracts/valid/VotingToken" => Ignore // too slow
    case _ => super.filter(ctx, name)
  }

  testAll("smartcontracts/valid", true) { (analysis, reporter) =>
    assert(analysis.toReport.stats.validFromCache == 0, "no cache should be used for these tests")
    for ((vc, vr) <- analysis.vrs) {
      if (vr.isInvalid) fail(s"The following verification condition was invalid: $vc @${vc.getPos}")
      if (vr.isInconclusive) fail(s"The following verification condition was inconclusive: $vc @${vc.getPos}")
    }
    reporter.terminateIfError()
  }

  testAll("smartcontracts/failVerification", true) { (analysis, _) =>
    val report = analysis.toReport
    assert(report.totalInvalid > 0, "There should be at least one invalid verification condition. " + report.stats)
  }
}

