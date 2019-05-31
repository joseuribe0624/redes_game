import java.io.{BufferedInputStream, BufferedOutputStream, ObjectInputStream, ObjectOutputStream}
import java.net.{InetAddress, InetSocketAddress, SocketException}


object SlaveTest {
  def main(args: Array[String]): Unit = {
    import java.net.Socket
    val socket = new Socket()
    socket.connect(new InetSocketAddress(InetAddress.getByName("192.168.121.18"), Server.PORT_ALIVE), 500)
    val oos = new ObjectOutputStream(new BufferedOutputStream(socket.getOutputStream))
    oos.flush()

    //val ois = new ObjectInputStream(new BufferedInputStream(socket.getInputStream))

    /*print()
    Thread.sleep(5000)
    oos.flush()
    oos.writeInt(51)*/

    //val jose = Helper.Server(InetAddress.getByAddress("puj.edu.co", Array[Byte](10.toByte, 5.toByte, 99.toByte, 216.toByte) ))
    //println(jose.ip.isReachable(5000))
    val ip = InetAddress.getByName("192.168.121.18")
    try {
      while (true) {
        if (!ip.isReachable(5)) throw new SocketException("Unreachable")
        oos.writeInt(5)
        oos.flush()
        println("AAAA")
        Thread.sleep(500)
        //println(ois.readObject())
      }
    } catch {
      case _: Throwable => println("Server is down")
    }

  }
}
