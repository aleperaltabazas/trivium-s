package cripto.mutable

import cripto.utils.Utility

case object Main {
  def main(args: Array[String]): Unit = {
    if (args.length < 4) {
      System.out.println("Run like ./trivium <key> <iv> <inputFile> <outputFile>")
      System.exit(-1)
    }

    val inputFile = args(args.length - 2)
    val outputFile = args(args.length - 1)
    val key = Utility.stringToHex(args(args.length - 4))
    val iv = Utility.stringToHex(args(args.length - 3))

    val cipher = Trivium(key, iv)

    val encryptor: FileEncrypt = FileEncrypt(inputFile, outputFile, cipher)
    encryptor.encrypt
    encryptor.close()
  }
}
