package cripto.immutable

import java.io._

case class ImmutableFileEncryption(var cipher: ImmutableTrivium, reader: DataInputStream, writer: DataOutputStream) {
  def encrypt(): Unit = {
    var readBytes: Int = 0
    val buffer = new Array[Byte](ImmutableFileEncryption.maxBufferSize)

    do {
      readBytes = reader.read(buffer, 0, ImmutableFileEncryption.maxBufferSize)

      for (i <- List.range(0, readBytes)) {
        val (byte, newCipher) = cipher.keyByte
        cipher = newCipher
        buffer(i) = (buffer(i) ^ byte).asInstanceOf[Byte]
      }

      if (readBytes > 0) {
        writer.write(buffer, 0, readBytes)
        writer.flush()
      }

    } while (readBytes > 0)
  }

  def close(): Unit = {
    reader.close()
    writer.close()
  }
}

case object ImmutableFileEncryption {
  private final val maxBufferSize: Int = 2048

  def apply(inputFilePath: String, outputFilePath: String, cipher: ImmutableTrivium): ImmutableFileEncryption = ImmutableFileEncryption(
    cipher,
    new DataInputStream(new FileInputStream(new File(inputFilePath))),
    new DataOutputStream(new FileOutputStream(new File(outputFilePath)))
  )
}