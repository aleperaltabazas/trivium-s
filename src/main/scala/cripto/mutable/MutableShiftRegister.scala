package cripto.mutable

class MutableShiftRegister(var sizeOfRegister: Int, var tap: Byte, var flipFlops: Array[MutableFlipFlop]) {
  def size: Int = this.flipFlops.length

  def getOutput: Byte = this.tap

  def getBitAt(position: Int): Byte = flipFlops(position).getValue

  def loadValue(value: Byte): Unit = {
    flipFlops(0).tick(value)
  }

  def getBits(positions: Int*): Array[Byte] = {
    val result = new Array[Byte](positions.length)

    for (i <- List.range(0, result.length - 1)) {
      result(i) = flipFlops(positions(i)).getValue
    }

    result
  }
}

case object MutableShiftRegister {
  def apply(sizeOfRegister: Int): MutableShiftRegister = {
    val flipFlops: Array[MutableFlipFlop] = new Array[MutableFlipFlop](sizeOfRegister)

    flipFlops(sizeOfRegister - 1) = MutableFlipFlop()

    var i: Int = flipFlops.length - 2

    while (i >= 0) {
      flipFlops(i) = MutableFlipFlop(flipFlops(i + 1))
      i -= 1
    }

    val tap: Byte = 0x00
    new MutableShiftRegister(sizeOfRegister, tap, flipFlops)
  }
}