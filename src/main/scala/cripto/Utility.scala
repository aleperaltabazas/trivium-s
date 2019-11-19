package cripto

object Utility {
  def stringToHex(string: String): Array[Byte] = {
    val s = string.replaceAll(" ", "")
    val length = s.length / 2
    var count = 0
    val result = new Array[Byte](length)
    var i = 0
    while ( {
      i < s.length
    }) {
      val temp = ((charToHex(s.charAt(i)) << 4) | charToHex(s.charAt(i + 1))).toByte
      result(count) = temp
      count += 1

      i = i + 2
    }
    result
  }

  def swapEndianness(bytes: Array[Byte]): Array[Byte] = {
    val result = new Array[Byte](bytes.length)
    var i = 0
    while ( {
      i < bytes.length
    }) {
      result(i) = bytes(bytes.length - 1 - i)

      {
        i += 1; i - 1
      }
    }
    result
  }

  def byteToHexString(b: Byte): String = {
    var result = ""
    val upperByte = ((b & 0xf0) >> 4).toByte
    result += byteToChar(upperByte)
    val lowerByte = (b & 0x0f).toByte
    result += byteToChar(lowerByte)
    result
  }

  private def byteToChar(c: Byte) = {
    var result = '0'
    if (c <= 9 && c >= 0) result = (c + '0').toChar
    if (c <= 15 && c >= 10) result = (c - 10 + 'A').toChar
    result
  }

  private def charToHex(c: Char) = {
    var result = 0x00
    if (c <= '9' && c >= '0') result = (c - '0').toByte
    if (c <= 'F' && c >= 'A') result = (c - 'A' + 10).toByte
    if (c <= 'f' && c >= 'a') result = (c - 'a' + 10).toByte
    result
  }
}