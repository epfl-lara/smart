package stainless

package object solidity {

  object optSolidityOutput extends inox.FlagOptionDef("solidity", false)

  abstract class SolidityOutputException(msg: String) extends Exception(msg)
  case class SolidityFragmentException(msg: String) extends SolidityOutputException(msg)

  // Solidity file name corresponding to a Scala file
  // either we replace the .scala extension at the end if it is there, or 
  // we add .sol at the end of the file name
  def scalaToSolName(filename: String): String = { 
    if (filename.endsWith(".scala"))
      filename.dropRight(6) + ".sol"
    else 
      filename + ".sol"
  }
}
