package cripto.immutable

case class FlipFlop(value: Byte = 0, rightNeighbor: Option[FlipFlop] = None) {
  def tick(value: Byte): FlipFlop = {
    FlipFlop(value, rightNeighbor.map(_.tick(this.value)))
  }

  def collect: List[FlipFlop] = List(this) ++ rightNeighbor.map(_.collect).getOrElse(Nil)
}