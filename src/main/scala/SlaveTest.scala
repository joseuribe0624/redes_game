import java.io.{BufferedInputStream, BufferedOutputStream, ObjectInputStream, ObjectOutputStream}

object SlaveTest {
  def main(args: Array[String]): Unit = {
    import java.net.Socket
    val socket = new Socket("10.5.99.145", 4445)

    val ois = new ObjectInputStream(new BufferedInputStream(socket.getInputStream()))
    val oos = new ObjectOutputStream(new BufferedOutputStream(socket.getOutputStream()))

    while (true) {
      println(ois.readObject())
    }
  }
}
