package cripto.mutable

case class MutableTrivium(registerOne: TriviumShiftRegister = TriviumShiftRegister(93),
                          registerTwo: TriviumShiftRegister = TriviumShiftRegister(84),
                          registerThree: TriviumShiftRegister = TriviumShiftRegister(111)) {
  initializationRounds()

  def getKeyByte: Byte = {
    var lowBits: Byte = 0x00
    var highBits: Byte = 0x00

    for (i <- List.range(0, 3)) {
      lowBits = (lowBits | (getKeyBit << i)).toByte
    }

    for (i <- List.range(0, 3)) {
      highBits = (highBits | (getKeyBit << i)).toByte
    }

    (highBits << 4 | lowBits).toByte
  }

  def getKeyBit: Byte = {
    val regOneBits = registerOne.getBits(65, 90, 91, 92, 68)
    val regTwoBits = registerTwo.getBits(68, 81, 82, 83, 77)
    val regThreeBits = registerThree.getBits(65, 108, 109, 110, 86)

    val result: Byte = (regOneBits(0) ^ regOneBits(3) ^ regTwoBits(0) ^ regTwoBits(3) ^ regThreeBits(0) ^ regThreeBits(3)).toByte
    var t1, t2, t3: Byte = 0x00

    t1 = (regOneBits(1) & regOneBits(2)).toByte
    t1 = (t1 ^ regTwoBits(4) ^ regOneBits(0) ^ regOneBits(3)).asInstanceOf[Byte]

    t2 = (regTwoBits(1) & regTwoBits(2)).toByte
    t2 = (t2 ^ regThreeBits(4) ^ regTwoBits(0) ^ regTwoBits(3)).asInstanceOf[Byte]

    t3 = (regThreeBits(1) & regThreeBits(2)).toByte
    t3 = (t3 ^ regOneBits(4) ^ regThreeBits(0) ^ regThreeBits(3)).asInstanceOf[Byte]

    registerOne.loadValue(t3)
    registerTwo.loadValue(t1)
    registerThree.loadValue(t2)

    result
  }

  private def initializationRounds(): Unit = {
    for (_ <- List.range(1, 4 * 288)) {
      getKeyBit
    }
  }
}

case object MutableTrivium {
  def apply(key: Array[Byte], iv: Array[Byte]): MutableTrivium = {
    val registerOne = TriviumShiftRegister(93)
    val registerTwo = TriviumShiftRegister(84)
    val registerThree = TriviumShiftRegister(111)

    initRegister(key, registerOne)
    initRegister(iv, registerTwo)
    registerThree.loadValue(0x01)
    registerThree.loadValue(0x01)
    registerThree.loadValue(0x01)

    List.range(0, registerThree.size - 3).foreach(_ => registerThree.loadValue(0x00))

    MutableTrivium(registerOne, registerTwo, registerThree)
  }

  private def initRegister(bytes: Array[Byte], register: TriviumShiftRegister): Unit = {
    bytes.reverse.foreach {
      b =>
        List.range(1, 8).foreach {
          i =>
            val temp = ((b >> i) & 1).asInstanceOf[Byte]
            register.loadValue(temp)
        }
    }
  }
}