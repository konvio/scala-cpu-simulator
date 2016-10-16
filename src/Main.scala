object Main extends App {

  val ac = new Register("ac")

  val registry = new Cluster(4, "r")
  val memory = new Cluster(4, "m")

  def printState() = {
    println("ac: " + ac)
    registry.print()
   // memory.print()
  }

  def load(arg: String, code: String, name:String) = {
    try {
      println(code)
      registry.add(ac.state)
      ac.load(arg.toInt)
      println(name)
    }
    catch {
      case e: NumberFormatException => println("Invalid argument: a number expected")
    }
  }

  def xor(arg: String, code: String, name:String) = {
    try {
      println(code)
      registry.add(ac.state)
      ac.xor(arg.toInt)
      println(name)
    }
    catch {
      case e: NumberFormatException => println("Invalid argument: a number expected")
    }
  }

  def apply(value: Int, code: String, name: String) = {
    println(code)
    println(name)
    registry.add(ac.state)
    ac.load(value)
  }

  def applyXor(value: Int, code: String, name: String) = {
    println(code)
    println(name)
    registry.add(ac.state)
    ac.xor(value)
  }

  while (true) {
    printState()
    val args = io.StdIn.readLine.trim.toLowerCase split "[^\\w']+"
    args match {
      case Array("load", "r1") => apply(registry.state(0).state, "001 load<registry>", "load<r1>")
      case Array("load", "r2") => apply(registry.state(1).state, "001 load<registry>", "load<r2>")
      case Array("load", "r3") => apply(registry.state(2).state, "001 load<registry>", "load<r3>")
      case Array("load", "r4") => apply(registry.state(3).state, "001 load<registry>", "load<r4>")

      case Array("load", "m1") => apply(memory.state(0).state, "002 load<memory>", "load<m1>")
      case Array("load", "m2") => apply(memory.state(1).state, "002 load<memory>", "load<m2>")
      case Array("load", "m3") => apply(memory.state(2).state, "002 load<memory>", "load<m3>")
      case Array("load", "m4") => apply(memory.state(3).state, "002 load<memory>", "load<m4>")
      case Array("load", _) => xor(args(1), "000 load<literal>", "load<" + args(1) + ">")

      case Array("xor", "r1") => applyXor(registry.state(0).state, "010 xor<registry>", "xor<r1>")
      case Array("xor", "r2") => applyXor(registry.state(1).state, "010 xor<registry>", "xor<r2>")
      case Array("xor", "r3") => applyXor(registry.state(2).state, "010 xor<registry>", "xor<r3>")
      case Array("xor", "r4") => applyXor(registry.state(3).state, "010 xor<registry>", "xor<r4>")

      case Array("xor", "m1") => applyXor(memory.state(0).state, "011 xor<memory>", "xor<m1>")
      case Array("xor", "m2") => applyXor(memory.state(1).state, "011 xor<memory>", "xor<m2>")
      case Array("xor", "m3") => applyXor(memory.state(2).state, "011 xor<memory>", "xor<m3>")
      case Array("xor", "m4") => applyXor(memory.state(3).state, "011 xor<memory>", "xor<m4>")

      case Array("xor", _) => xor(args(1), "012 xor<literal>", "xor<" + args(1) + ">")


      case Array("exit") => System.exit(0)
      case _ => println("Invalid operation")
    }
  }
}
