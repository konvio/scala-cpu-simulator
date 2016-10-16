class Cluster(size: Int, nameId: String) {

  require()

  val state = new Array[Register](size)
  for(i <- 0 until size) state(i) = new Register(nameId + (i + 1))

  def add(value: Int) = {
    for (i <- 1 until size) state(i).load(state(i-1).state)
    state(0).load(value)
  }

  def print() = for(reg <- state) println(reg.name + ": " + reg)
}
