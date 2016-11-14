object Main extends App {

  val ac = new Register("ac")
  val regs = new Array[Register](4)
  val regR = "r([0-3])".r
  val numR = "(\\-?\\d+)".r

  def displayState() = {
    println(ac.getFlags())
    println(ac)
    for (r <- regs) println(r)
  }

  def load(arg: String) = {
    arg match {
      case regR(index) => ac load regs(index.toInt)
      case numR(value) => ac load value.toInt
      case _ => println("Invalid argument")
    }
  }

  def xor(arg: String) = {
    arg match {
      case regR(index) => ac xor regs(index.toInt)
      case numR(value) => ac xor value.toInt
      case _ => println("Invalid argument")
    }
  }

  def add(arg: String) = {
    arg match {
      case regR(index) => ac add regs(index.toInt)
      case numR(value) => ac add value.toInt
      case _ => println("Invalid argument")
    }
  }

  def copy(arg: String) = {
    arg match {
      case regR(index) => regs(index.toInt) copy ac
      case _ => println("Invalid argument")
    }
  }

  for (i <- regs.indices) regs(i) = new Register("r" + i)

  while (true) {
    displayState()
    val command = io.StdIn.readLine.trim.toLowerCase split "\\s+"
    command match {
      case Array("load", _) => load(command(1))
      case Array("xor", _) => xor(command(1))
      case Array("copy", _) => copy(command(1))
      case Array("add", _) => add(command(1))
      case Array("exit") => System.exit(0)
      case _ => println("Invalid operation")
    }
  }
}
