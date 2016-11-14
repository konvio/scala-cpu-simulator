class Register(val name: String) {

  var state = 0
  var of = 0
  var sf = 0
  var pf = 1

  def load(value: Int) = {
    if (value < Register.min || value > Register.max) {
      of = 1
      state = Register.normalizeRange(value)
    } else {
      of = 0
      state = value
    }
    sf = if (state < 0) 1 else 0
    pf = parityFlag()
  }

  def load(other: Register) = state = other.state

  def xor(value: Int) = {
    state ^= Register.normalizeRange(value)
    of = 0
    sf = if (state < 0) 1 else 0
    pf = parityFlag()
  }

  def xor(other: Register): Unit = xor(other.state)

  def add(value: Int) = {
    state += value
    if (state < Register.min || state > Register.max) {
      of = 1
      state = Register.normalizeRange(state)
    } else {
      of = 0
    }
    sf = if (state < 0) 1 else 0
    pf = parityFlag()
  }

  def add(other: Register): Unit = add(other.state)

  def copy(other: Register) = {
    state = other.state
    sf = other.sf
  }

  def getFlags() = s"OF:$of SF:$sf PF:$pf"

  def parityFlag() = {
    var count = 0
    for (c <- toString) {
      if (c == '1') count += 1
    }
    for (c <- name) {
      if (c == '1') count -= 1
    }
    if (count % 2 == 1) 0 else 1
  }

  override def toString = {
    //    val sign = if (state.toBinaryString.length < 32) "0" else "1"
    val sign = sf
    val realBits = name + ": " + Register.formattedBinaryString(state.toBinaryString, 24)
    realBits.substring(0, 4) + sign + realBits.substring(5)
  }
}

object Register {
  val max = (1 << 23) - 1
  val min = -(1 << 23)
  val unsignedMax = (1 << 24) - 1

  def normalizeRange(value: Int) = {
    var v = value
    while (v > max) v -= unsignedMax
    while (v < min) v += unsignedMax
    v
  }

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