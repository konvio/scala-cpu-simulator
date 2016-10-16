class Register(val name: String) {
  val bits = 24
  var state = 0

  def load(value: Int) {
    state = value
  }

  def xor(other: Int) {
    state = state ^ other
  }

  override def toString: String = Register.formattedBinaryString(state.toBinaryString, bits)
}

object Register {

  def formattedBinaryString(binaryString: String, desiredLength: Int): String =
    insertSpaces(formatLength(binaryString, desiredLength), 4)

  def formatLength(text: String, desiredLength: Int): String = {
    if (text.length == desiredLength) text
    else if (text.length > desiredLength) text.substring(text.length - desiredLength)
    else "0" * (desiredLength - text.length) + text
  }

  def insertSpaces(text: String, blockLength: Int): String = {
    if (text.length <= blockLength) text
    else {
      val (start, end) = text.splitAt(text.length - 4)
      insertSpaces(start, blockLength) + " " + end
    }
  }
}