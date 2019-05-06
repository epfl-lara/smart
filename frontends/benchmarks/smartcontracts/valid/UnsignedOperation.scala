import stainless.smartcontracts._
import stainless.annotation._

trait UnsignedOperation extends Contract {
	@solidityPure
	@solidityPublic
	def safe_add(x: Uint256, y: Uint256) = {
		dynRequire(unsafe_+(x,y) >= x)
		x + y
	}

	@solidityPure
	@solidityPublic
	def safe_minus(x: Uint256, y: Uint256) = {
		dynRequire(unsafe_-(x,y) <= x)
		x - y
	}

	@solidityPure
	@solidityPublic
	def safe_times(x: Uint256, y: Uint256) = {
		dynRequire(
			x == Uint256.ZERO ||
			y == unsafe_*(x,y) / x
		)
		x * y
	}

	@solidityPure
	@solidityPublic
	def safe_div(x: Uint256, y: Uint256) = {
		dynRequire(y > Uint256.ZERO)
		x / y
	}
}
