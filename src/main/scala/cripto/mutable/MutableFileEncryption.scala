package cripto.mutable

import java.io._

case class MutableFileEncryption(cipher: MutableTrivium, reader: DataInputStream, writer: DataOutputStream) {
  def encrypt(): Unit = {
    var readBytes: Int = 0
    val buffer = new Array[Byte](MutableFileEncryption.maxBufferSize)

    do {
      readBytes = reader.read(buffer, 0, MutableFileEncryption.maxBufferSize)

      for (i <- List.range(0, readBytes)) {
        buffer(i) = (buffer(i) ^ cipher.getKeyByte).asInstanceOf[Byte]
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

case object MutableFileEncryption {
  private final val maxBufferSize: Int = 2048

  def apply(inputFilePath: String, outputFilePath: String, cipher: MutableTrivium): MutableFileEncryption = MutableFileEncryption(
    cipher,
    new DataInputStream(new FileInputStream(new File(inputFilePath))),
    new DataOutputStream(new FileOutputStream(new File(outputFilePath)))
  )
}