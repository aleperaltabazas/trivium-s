package cripto

import cripto.immutable.{ImmutableFileEncryption, ImmutableTrivium}
import cripto.mutable.{MutableFileEncryption, MutableTrivium}
import cripto.utils.Utility

case object Main {

  def runImmutable(key: Array[Byte], iv: Array[Byte], input: String, output: String): Unit = {
    val cipher = ImmutableTrivium(key, iv)

    val encryption = ImmutableFileEncryption(input, output, cipher)
    val start = System.currentTimeMillis()
    encryption.encrypt()
    val diff = System.currentTimeMillis() - start
    encryption.close()
    System.out.println(s"Ciphered in $diff millis")
  }

  def runMutable(key: Array[Byte], iv: Array[Byte], input: String, output: String): Unit = {
    val cipher = MutableTrivium(key, iv)

    val encryption = MutableFileEncryption(input, output, cipher)
    val start = System.currentTimeMillis()
    encryption.encrypt()
    val diff = System.currentTimeMillis() - start
    encryption.close()
    System.out.println(s"Ciphered in $diff millis")
  }

  def main(args: Array[String]): Unit = {
    if (args.length < 4) {
      System.out.println("Run like ./trivium <key> <iv> <inputFile> <outputFile>")
      System.exit(-1)
    }

    val inputFile = args(args.length - 2)
    val outputFile = args(args.length - 1)
    val key = Utility.stringToHex(args(args.length - 4))
    val iv = Utility.stringToHex(args(args.length - 3))

    if (args.contains("-i")) {
      System.out.println("RUNNING IMMUTABLE")
      runImmutable(key, iv, inputFile, outputFile)
    } else {
      runMutable(key, iv, inputFile, outputFile)
    }

  }
}
