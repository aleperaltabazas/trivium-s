package cripto.immutable

case class ImmutableShiftRegister(registerSize: Int, tap: Byte, flipFlops: Array[ImmutableFlipFlop]) {
  def size: Int = this.flipFlops.length

  def output: Byte = tap

  def bitAt(position: Int): Byte = flipFlops(position).value

  def loadValue(value: Byte): ImmutableShiftRegister = copy(flipFlops = this.flipFlops(0).tick(value).collect.toArray)

  def bits(positions: Int*): Array[Byte] = positions.map(i => flipFlops(i)).map(_.value).toArray
}

case object ImmutableShiftRegister {
  def apply(registerSize: Int): ImmutableShiftRegister = {
    val flipFlops_ = List.fill(registerSize)(2).foldLeft((List[ImmutableFlipFlop](), ImmutableFlipFlop())) {
      (acc, _) =>
        val flipFlop = ImmutableFlipFlop(rightNeighbor = Some(acc._2))
        (flipFlop :: acc._1, flipFlop)
    }._1

    ImmutableShiftRegister(registerSize, 0x00, flipFlops_.toArray)
  }
}