import java.io.IOException
import java.net._

import Server.PORT_ALIVE

import scala.collection.mutable

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
    Server(InetAddress.getByAddress(Array[Byte](192.toByte, 168.toByte, 121.toByte, 40.toByte) )),
    Server(InetAddress.getByAddress(Array[Byte](192.toByte, 168.toByte, 121.toByte, 38.toByte) )),
    // Server(InetAddress.getByAddress(Array[Byte](10.toByte, 5.toByte, 99.toByte, 207.toByte) )),
  )

  def checkServer(server: Server): Boolean = {
    try {
      val s = new Socket()
      s.connect(new InetSocketAddress(server.ip, PORT_ALIVE), 500)
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

  def main(args: Array[String]): Unit = {

    val check_master: Runnable = new Runnable {
      override def run(): Unit = {
        Thread.sleep(3000)
        throw new SocketException("Unreachable")
      }
    }

    val t: Thread = new Thread(check_master)

    t.setUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler {
      override def uncaughtException(t: Thread, e: Throwable): Unit = {
        throw new SocketException("Unreachable")
      }
    })
    try {

      t.start()
    } catch {
      case _: Throwable => print("Catch")
    }
    //val test = InetAddress.getByAddress(Array[Byte](192.toByte, 168.toByte, 121.toByte, 18.toByte) )
    //println(master)
    //println(getMasterAvailableServer())
  }
}
