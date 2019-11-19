package cripto.immutable

case class ImmutableFlipFlop(value: Byte = 0, rightNeighbor: Option[ImmutableFlipFlop] = None) {
  def tick(value: Byte): ImmutableFlipFlop = {
    ImmutableFlipFlop(value, rightNeighbor.map(_.tick(this.value)))
  }

  def collect: List[ImmutableFlipFlop] = List(this) ++ rightNeighbor.map(_.collect).getOrElse(Nil)
}