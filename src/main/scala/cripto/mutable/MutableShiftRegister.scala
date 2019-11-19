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
  def apply_(sizeOfRegister: Int): MutableShiftRegister = {
    var flipFlops: Array[MutableFlipFlop] = new Array[MutableFlipFlop](sizeOfRegister)

    flipFlops(sizeOfRegister - 1) = MutableFlipFlop()

    var i: Int = flipFlops.length - 2
    while ( {
      i >= 0
    }) {
      flipFlops(i) = MutableFlipFlop(flipFlops(i + 1))

      {
        i -= 1;
        i + 1
      }
    }
    val tap: Byte = 0x00
    new MutableShiftRegister(sizeOfRegister, tap, flipFlops)
  }

  def apply(registerSize: Int): MutableShiftRegister = {
    val flipFlops: Array[MutableFlipFlop] = new Array[MutableFlipFlop](registerSize)

    flipFlops(registerSize - 1) = MutableFlipFlop()

    val flipFlops_ = List.fill(registerSize)(2).foldLeft((List[MutableFlipFlop](), MutableFlipFlop())) {
      (acc, _) =>
        val flipFlop = MutableFlipFlop(acc._2)
        (flipFlop :: acc._1, flipFlop)
    }._1

    for (i <- 10 to 1) {
      flipFlops(i) = MutableFlipFlop(flipFlops(i + 1))
    }

    new MutableShiftRegister(registerSize, 0x00, flipFlops_.toArray)
  }
}