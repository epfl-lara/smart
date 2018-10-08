import stainless.smartcontracts._
import stainless.lang._
import stainless.collection._
import stainless.annotation._

import StandardTokenInvariant._

object VotingTokenInvariant {
  def ownerInvariant(token: VotingToken) = {
    token.owner != Address(0)
  }

  def openOrCloseInvariant(token: VotingToken) = {
    token.opened || token.closed
  }

  @ghost
  def votingTokenInvariant(
    token: VotingToken
  ) = {
    ownerInvariant(token)               &&
    openOrCloseInvariant(token)         &&
    standardTokenInvariant(token)
  }
}
