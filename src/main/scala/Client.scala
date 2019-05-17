import scala.swing._
import java.awt.image.BufferedImage
import java.awt.Color
import java.io.{BufferedInputStream, BufferedOutputStream, IOException, ObjectInputStream, ObjectOutputStream}
import java.net.{InetAddress, Socket}

import scala.swing.event._
import java.awt.Graphics2D
import java.awt.geom.Rectangle2D

import scala.collection.mutable.ArrayBuffer



object Client {
  case class TurnLeft(num:Int)
  case class TurnRight(num:Int)
  case class InitPlayer(id: Int)
  case class AvailableServer(ip: InetAddress) {
    def max(that: AvailableServer): AvailableServer = {
      val ipSelf = this.ip.getAddress()
      val ipThat = that.ip.getAddress()

      return this
    }
  }
  val servers = List(
    AvailableServer(InetAddress.getByName("10.5.99.176")),
    AvailableServer(InetAddress.getByName("10.5.99.196")),
  )

  val sock = new Socket("localhost", 4444)
  val ois = new ObjectInputStream(new BufferedInputStream(sock.getInputStream()))
  val oos = new ObjectOutputStream(new BufferedOutputStream(sock.getOutputStream()))
  var playerNumber = 0
  oos.flush
  var message = ""
  //var draw: Rectangle2D = new Rectangle2D.Double(0,0 ,0,0)
  var squares: ArrayBuffer[Rectangle2D] = new  ArrayBuffer[Rectangle2D]
  var lines: ArrayBuffer[Rectangle2D] = new  ArrayBuffer[Rectangle2D]

  var img: BufferedImage = new BufferedImage(500,500, BufferedImage.TYPE_INT_RGB)
  var one = false
  val panel = new Panel {
    override def paint(g: Graphics2D){
      g.drawImage(img, 0, 0, null)
      g.setPaint(Color.white)
      g.drawString(message, 200,250)

      for (read <- 0 to squares.length - 1) {
        g.fill(squares(read))
      }
      for (readL <- 0 to lines.length - 1) {
        g.fill(lines(readL))
      }
    }

    preferredSize = new Dimension(img.getWidth, img.getHeight)
    listenTo(mouse.clicks, keys)
    reactions += {
      case e: MouseClicked => requestFocus
      case e: MouseEntered => requestFocus
      case e: KeyPressed =>
        e.key match {
          case Key.Left =>
            oos.writeObject(TurnLeft(playerNumber))
            oos.flush
          case Key.Right =>
            oos.writeObject(TurnRight(playerNumber))
            oos.flush
          case _ =>
        }
    }
  }

  val frame = new MainFrame {
    title = "Tron Race"
    contents = panel
    centerOnScreen
  }

  def main(args: Array[String]){
    frame.open
    panel.requestFocus
    var flag = true
    while(flag){
      ois.readObject match {
        case InitPlayer(id) => initPlayer(id)
        case Server.CountDown(value) => gameStart(value)
        //tener en cuenta esto a la hora de la cant de jugadores
        case Server.GameDrawRoad(obst) => gameDrawRoad(obst)
        case Server.StepTaken(p1,p2) => stepTaken(p1,p2)
        case Server.GameEnds(winner) => gameEnds(winner)
          flag= false
      }
    }
  }
  def gameStart(count:Int){
    message = count.toString
    //draw.setRect(70,250,10,5)
    if(panel != null) panel.repaint
  }


  def gameDrawRoad(obst: ArrayBuffer[Rectangle2D]){
    var x = 40
    var y = 48
    var cantLines = 0
    var yRoad = 20*(-1)
    while (cantLines < 8) {
      var lines1: Rectangle2D = new Rectangle2D.Double(0, 0, 0, 0)
      lines1.setRect(x, y+yRoad, 400, 2)
      lines.append(lines1)
      cantLines = cantLines + 1
      yRoad = yRoad * (-1)
      if (cantLines % 2 == 0 ) {
        yRoad = 20
        y = y + 50
      }
    }
    squares = obst
    panel.repaint
  }
  def initPlayer(id: Int): Unit ={
    playerNumber = id
  }

  def gameEnds(winner: Int){
    message = if(winner == playerNumber)"You won!" else "You lost."
    if (panel != null) panel.repaint
  }

  def stepTaken(p1: Seq[(Int, Int)], p2: Seq[(Int, Int)]){
    message = ""

    for ((x,y) <- p1) img.setRGB(x,y, Color.red.getRGB())
    for ((x,y) <- p2) img.setRGB(x,y, Color.blue.getRGB())
    //for ((x,y) <- p1Map) img.setRGB(x,y, Color.green.getRGB)
    panel.repaint
  }

}
