package cripto.mutable

class MutableFlipFlop(var value: Byte = 0, var hasRightNeighbor: Boolean = false, var rightNeighbor: MutableFlipFlop = null) {
  def tick(value: Byte): Unit = {
    if (hasRightNeighbor) rightNeighbor.tick(this.value)
    this.value = value
  }

  def getRightNeighbor: MutableFlipFlop = rightNeighbor

  def getValue: Byte = value
}

case object MutableFlipFlop {
  def apply(): MutableFlipFlop = new MutableFlipFlop()

  def apply(neighbor: MutableFlipFlop): MutableFlipFlop = new MutableFlipFlop(0, true, neighbor)
}