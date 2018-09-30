package stainless
package soliditycompiler

object optSolidityCompiler extends inox.FlagOptionDef("solidity", false)

abstract class SolidityCompilerException(msg: String) extends Exception(msg)
case class SolidityFragmentException(msg: String) extends SolidityCompilerException(msg)
// case class SolidityFragmentException(msg) extends SolidityCompilerException(msg)
