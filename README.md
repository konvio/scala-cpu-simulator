######Кононенко Віталій, К-24
#Лабораторна робота №2
##Імітаційна робота процесора
 
###Огляд
Розроблена програмна модель процесора та реалізована його імітаційна модель.
![demo](http://i.imgur.com/fbhdGoR.png)

###Варіант
* Адресність: 1-адресний процесор
* Бітність: 24-бітні дані
* Команда: xor

###Регістри
Доступні 4 регістри r0, r1, r2, r3 та акумулятор ac. Оскільки процесор однобітний,
всі операції за замовчування відбуваються в акумуляторі.

###Команди
* **load** - завантажити число в акумулятор
* **copy** - копіювати стан акумулятора в один з регістрів
* **add** - додади до акумулятора число або значення одного з регістрів
* **xor** - виконати додавання за модулем 2 до акумулятора числа або значення одного з регістрів
* **exit** - завершити програму

###Scala
Для реалізації програмної моделі використана мова програмування [Scala](http://www.scala-lang.org/).
Scala компілюціються в байткод JVM та повністю сумісна з Java (класи обох можна спільно використовувати).

###Код
```scala
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
```
```scala
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
```
######© 2016