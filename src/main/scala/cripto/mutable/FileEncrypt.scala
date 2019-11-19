package cripto.mutable

import java.io._

case class FileEncrypt(cipher: Trivium, reader: DataInputStream, writer: DataOutputStream) {
  def encrypt: Unit = {
    var readBytes: Int = 0
    var buffer = new Array[Byte](FileEncrypt.maxBufferSize)

    do {
      readBytes = reader.read(buffer, 0, FileEncrypt.maxBufferSize)

      for (i <- List.range(0, readBytes)) {
        buffer(i) = (buffer(i) ^ cipher.getKeyBites).asInstanceOf[Byte]
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

case object FileEncrypt {
  private final val maxBufferSize: Int = 2048

  def apply(inputFilePath: String, outputFilePath: String, cipher: Trivium): FileEncrypt = FileEncrypt(
    cipher,
    new DataInputStream(new FileInputStream(new File(inputFilePath))),
    new DataOutputStream(new FileOutputStream(new File(outputFilePath)))
  )
}