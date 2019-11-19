package cripto.utils

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

  private def charToHex(c: Char) = {
    var result = 0x00
    if (c <= '9' && c >= '0') result = (c - '0').toByte
    if (c <= 'F' && c >= 'A') result = (c - 'A' + 10).toByte
    if (c <= 'f' && c >= 'a') result = (c - 'a' + 10).toByte
    result
  }
}