package stainless
package solidity

object optSolidityOutput extends inox.FlagOptionDef("solidity", false)

abstract class SolidityOutputException(msg: String) extends Exception(msg)
case class SolidityFragmentException(msg: String) extends SolidityOutputException(msg)
// case class SolidityFragmentException(msg) extends SolidityOutputException(msg)
