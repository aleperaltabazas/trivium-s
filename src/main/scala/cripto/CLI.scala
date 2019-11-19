package cripto

case class CLI(cipher: Trivium) {
  def run(input: String, output: String): Unit = {
    val encryptor: FileEncrypt = FileEncrypt(input, output, cipher)
    encryptor.encrypt
    encryptor.close
  }
}

case object CLI {
  def apply(args: Array[String]): Unit = {
    if (args.length < 4) {
      System.out.println("Run like ./trivium <key> <iv> <inputFile> <outputFile>")
      System.exit(-1)
    }

    val inputFile = args(args.length - 2)
    val outputFile = args(args.length - 1)
    val key = Utility.stringToHex(args(args.length - 4))
    val iv = Utility.stringToHex(args(args.length - 3))

    val cipher = Trivium(key, iv)
    CLI(cipher).run(inputFile, outputFile)
  }
}