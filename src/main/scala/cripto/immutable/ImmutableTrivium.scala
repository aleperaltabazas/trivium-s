package cripto.immutable

case class ImmutableTrivium(registerOne: TriviumShiftRegister = TriviumShiftRegister(93),
                            registerTwo: TriviumShiftRegister = TriviumShiftRegister(84),
                            registerThree: TriviumShiftRegister = TriviumShiftRegister(111)) {
  def initializationRounds: ImmutableTrivium = List.range(1, 4 * 288).foldLeft(this) {
    (trivium, _) => trivium.keyBit._2
  }

  def keyByte: (Byte, ImmutableTrivium) = {
    var lowBits: Byte = 0x00
    var highBits = 0x00
    var self = this

    for (i <- List.range(0, 3)) {
      val (bit, newSelf) = self.keyBit
      lowBits = (lowBits | (bit << i)).toByte
      self = newSelf
    }

    for (i <- List.range(0, 3)) {
      val (bit, newSelf) = self.keyBit
      highBits = (highBits | (bit << i)).toByte
      self = newSelf
    }

    ((highBits << 4 | lowBits).toByte, self)
  }

  def keyBit: (Byte, ImmutableTrivium) = {
    val regOneBits = this.registerOne.bits(65, 90, 91, 92, 68)
    val regTwoBits = this.registerTwo.bits(68, 81, 82, 83, 77)
    val regThreeBits = this.registerThree.bits(65, 108, 109, 110, 86)

    val z = (regOneBits(0) ^ regOneBits(3) ^ regTwoBits(0) ^ regTwoBits(3) ^ regThreeBits(0) ^ regThreeBits(3)).toByte

    var t1, t2, t3: Byte = 0x00

    t1 = (regOneBits(1) & regOneBits(2)).toByte
    t1 = (t1 ^ regTwoBits(4) ^ regOneBits(0) ^ regOneBits(3)).asInstanceOf[Byte]

    t2 = (regTwoBits(1) & regTwoBits(2)).toByte
    t2 = (t2 ^ regThreeBits(4) ^ regTwoBits(0) ^ regTwoBits(3)).asInstanceOf[Byte]

    t3 = (regThreeBits(1) & regThreeBits(2)).toByte
    t3 = (t3 ^ regOneBits(4) ^ regThreeBits(0) ^ regThreeBits(3)).asInstanceOf[Byte]

    val registerOne = this.registerOne.loadValue(t3)
    val registerTwo = this.registerTwo.loadValue(t1)
    val registerThree = this.registerThree.loadValue(t2)

    (z, ImmutableTrivium(registerOne, registerTwo, registerThree))
  }
}

case object ImmutableTrivium {
  def apply(key: Array[Byte], iv: Array[Byte]): ImmutableTrivium = {
    val registerOne = firstRegister(key)
    val registerTwo = secondRegister(iv)
    val registerThree = thirdRegister

    ImmutableTrivium(registerOne, registerTwo, registerThree)
  }

  private def firstRegister(key: Array[Byte]): TriviumShiftRegister = {
    val registerOne = TriviumShiftRegister(93)
    initRegisterWithByteArray(key, registerOne)
  }

  private def secondRegister(iv: Array[Byte]): TriviumShiftRegister = {
    val registerTwo = TriviumShiftRegister(84)
    initRegisterWithByteArray(iv, registerTwo)
  }

  private def thirdRegister: TriviumShiftRegister = {
    val registerThree = TriviumShiftRegister(111)

    val registerThreeV2 = registerThree.loadValue(0x01).loadValue(0x01).loadValue(0x01)

    List.range(0, registerThreeV2.size - 3).foldLeft(registerThreeV2) {
      (register, _) => register.loadValue(0x00)
    }
  }

  private def initRegisterWithByteArray(array: Array[Byte], register: TriviumShiftRegister): TriviumShiftRegister = array.reverse.foldLeft(register) {
    (register_, b) =>
      List.range(0, 7).foldLeft(register_) {
        (_register, i) =>
          val temp: Byte = ((b >> i) & 1).asInstanceOf[Byte]
          _register.loadValue(temp)
      }
  }
}