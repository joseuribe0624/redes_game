import java.io.IOException
import java.net.{InetAddress, InetSocketAddress, Socket, SocketTimeoutException}

object Helper {
  case class Server(ip: InetAddress) {
    def >(that: Server): Boolean = {
      val ipSelf = this.ip.getHostName
      val ipThat = that.ip.getHostName

      ipSelf > ipThat
    }

    def <(that: Server): Boolean = {
      val ipSelf = this.ip.getHostName
      val ipThat = that.ip.getHostName

      ipSelf < ipThat
    }

    def max(that: Server): Server = {
      if (that > this) {
        return that
      }
      this
    }
  }

  val servers = List(
    Server(InetAddress.getByAddress("puj.edu.co", Array[Byte](10.toByte, 5.toByte, 99.toByte, 216.toByte) )),
    Server(InetAddress.getByAddress("puj.edu.co", Array[Byte](10.toByte, 5.toByte, 99.toByte, 145.toByte) )),
  )

  def checkServer(server: Server): Boolean = {
    try {
      val s = new Socket()
      s.connect(new InetSocketAddress(server.ip, 4446), 500)
      s.close()
      true
    } catch {
      case _: SocketTimeoutException => false
      case _: IOException => false
    }
  }

  def getMasterAvailableServer(): Server = {
    val available = servers.filter(checkServer)
    if (available.isEmpty) return Server(null)
    available.reduce((X, Y) => X max Y)
  }
}
