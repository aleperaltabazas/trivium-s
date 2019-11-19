package cripto

class FlipFlop(var value: Byte = 0, var hasRightNeighbor: Boolean = false, var rightNeighbor: FlipFlop = null) {
  def tick(value: Byte): Unit = {
    if (hasRightNeighbor) rightNeighbor.tick(this.value)
    this.value = value
  }

  def getRightNeighbor: FlipFlop = rightNeighbor

  def getValue: Byte = value
}

case object FlipFlop {
  def apply(): FlipFlop = new FlipFlop()

  def apply(neighbor: FlipFlop): FlipFlop = new FlipFlop(0, true, neighbor)
}