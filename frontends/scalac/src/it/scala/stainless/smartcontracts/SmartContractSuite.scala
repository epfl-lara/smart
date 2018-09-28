/* Copyright 2009-2018 EPFL, Lausanne */

package stainless
package smartcontracts

import utils._

import org.scalatest._

trait SmartContractSuite extends FunSuite with inox.ResourceUtils with InputUtils {
  val validFiles = resourceFiles("smartcontracts/valid", _.endsWith(".scala"), false).map(_.getPath).toSeq
  val validDirs = subdirectories(getClass.getResource("/smartcontracts/valid").getPath)
  val validArgs = validFiles.map(Array(_)) ++ validDirs.map(d => files(d, _.endsWith(".scala")).toArray)
}