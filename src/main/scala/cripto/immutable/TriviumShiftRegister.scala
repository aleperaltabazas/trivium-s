package cripto.immutable

case class TriviumShiftRegister(registerSize: Int, tap: Byte, flipFlops: Array[FlipFlop]) {
  def size: Int = this.flipFlops.length

  def output: Byte = tap

  def bitAt(position: Int): Byte = flipFlops(position).value

  def loadValue(value: Byte): TriviumShiftRegister = copy(flipFlops = this.flipFlops(0).tick(value).collect.toArray)

  def bits(positions: Int*): Array[Byte] = {
    val result = new Array[Byte](positions.length)

    for (i <- List.range(0, result.length - 1)) {
      result(i) = flipFlops(positions(i)).value
    }

    result
  }
}

case object TriviumShiftRegister {
  def apply(registerSize: Int): TriviumShiftRegister = {
    val flipFlops_ = List.fill(registerSize)(2).foldLeft((List[FlipFlop](), FlipFlop())) {
      (acc, _) =>
        val flipFlop = FlipFlop(rightNeighbor = Some(acc._2))
        (flipFlop :: acc._1, flipFlop)
    }._1

    TriviumShiftRegister(registerSize, 0x00, flipFlops_.toArray)
  }
}