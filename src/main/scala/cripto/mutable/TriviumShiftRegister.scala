package cripto.mutable

class TriviumShiftRegister(var sizeOfRegister: Int, var tap: Byte, var flipFlops: Array[FlipFlop]) {
  def size: Int = this.flipFlops.length

  def getOutput: Byte = this.tap

  def getBitAt(position: Int): Byte = flipFlops(position).getValue

  def loadValue(value: Byte): Unit = {
    flipFlops(0).tick(value)
  }

  def getBits(positions: Int*): Array[Byte] = {
    val result = new Array[Byte](positions.length)
    var i = 0
    while ( {
      i < result.length
    }) {
      result(i) = flipFlops(positions(i)).getValue

      {
        i += 1;
        i - 1
      }
    }
    result
  }
}

case object TriviumShiftRegister {
  def apply(sizeOfRegister: Int): TriviumShiftRegister = {
    var flipFlops: Array[FlipFlop] = new Array[FlipFlop](sizeOfRegister)

    flipFlops(sizeOfRegister - 1) = new FlipFlop

    var i: Int = flipFlops.length - 2
    while ( {
      i >= 0
    }) {
      flipFlops(i) = FlipFlop(flipFlops(i + 1))

      {
        i -= 1;
        i + 1
      }
    }
    val tap: Byte = 0x00
    new TriviumShiftRegister(sizeOfRegister, tap, flipFlops)
  }
}