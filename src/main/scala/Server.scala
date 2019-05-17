import collection.mutable
import java.net.ServerSocket
import java.io.ObjectInputStream
import java.net.Socket
import java.io.ObjectOutputStream
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.awt.Graphics2D
import java.awt.geom.Rectangle2D

import scala.collection.mutable.ArrayBuffer



object Server{
  case class CountDown(value:Int)
  //agregue p1Map
  case class StepTaken(p1: Seq[(Int, Int)], p2: Seq[(Int, Int)])
  case class GameEnds(winner:  Int)
  case class GameDrawRoad(rectangles:ArrayBuffer[Rectangle2D])
  var obstacles: ArrayBuffer[Rectangle2D] = new  ArrayBuffer[Rectangle2D]
  var linesRoad: ArrayBuffer[Rectangle2D] = new  ArrayBuffer[Rectangle2D]
  private class Player(val sock: Socket, val ois: ObjectInputStream, val oos: ObjectOutputStream,
                       var x: Int, var y: Int, var dir: Int, var x2: Int, var y2: Int)

  //map
  //class Maps(val sock: Socket, var x:Int, var y: Int)
  private val clients = mutable.Buffer[Player]()
  //private val obstacles = mutable.Buffer[Maps]()
  private var winner = -1

  def main(args: Array[String]){
    val ss= new ServerSocket(4444)
    while (clients.length < 2){
      val sock = ss.accept
      connect(sock)
    }
    startGame
  }

  def connect(sock: Socket){
    val oos = new ObjectOutputStream(new BufferedOutputStream(sock.getOutputStream()))
    oos.flush
    val ois = new ObjectInputStream(new BufferedInputStream(sock.getInputStream()))
    clients += new Player(sock, ois, oos, 10, if (clients.isEmpty) 50 else 100, 1, 70 , if (clients.isEmpty) 50 else 100 )

    val T = new ClientHandler(winner, ois, oos)
    T.start()
    oos.writeObject(Client.InitPlayer(clients.length - 1))
    oos.flush()
  }

  def readPlayers{
    for(c <- clients){
      if(c.ois.available()>0){
        c.ois.readObject match {
          case Client.TurnLeft(p) =>
            turnLeft(p)
          case Client.TurnRight(p)=>
            turnRight(p)
        }
      }
    }
  }
  def DrawRoad(){
    var x = 40
    var y = 48
    var reps = 0
    var obstaculos = 0
    var h=10
    var change = 0
    while (obstaculos < 12) {
      var draw2: Rectangle2D = new Rectangle2D.Double(0, 0, 0, 0)
      draw2.setRect(x, y, 10, h)
      obstacles.append(draw2)
      obstaculos = obstaculos + 1
      reps = reps + 1
      y = y + 50
      if (reps == 4) {
        h = 10
        x = x + 150
        y = 48
        reps = 0
        change = change + 1
      }
      if (change == 1) {
        change = 0
        h = 20
        y = 40
      }
    }

  }
  def turnLeft(player: Int){
    clients(player).dir = (clients(player).dir +3)%4
  }

  def turnRight(player: Int){
    clients(player).dir = (clients(player).dir +1)%4
  }

  def lose(playerNumber:Int): Boolean ={
    if (clients(0).x == 400 || clients(playerNumber).y == clients(1).y2 + 20 || clients(playerNumber).y == clients(1).y2-20){
      return true
    }
    else{
      return false
    }
  }

  def checkPos(actualPos: Int): Int ={
    var check = 30
    if (actualPos < check + 150 ){
      return 0
    }
    else if(actualPos < check + 150*2){
      return 1
    }
    else{
      return 2
    }
  }

  def collide(rect: Rectangle2D, playerNumber:Int): Boolean ={
    if(rect.getX < clients(playerNumber).x && clients(playerNumber).x < rect.getX + rect.getWidth
      && rect.getY < clients(playerNumber).y && clients(playerNumber).y < rect.getY + rect.getHeight ){
      return true
    }
    else{
      return false
    }
  }

  // esto lo que me dice es la dir a la que va a ir
  private def move(p: Player){
    p.dir match {
      case 0 => p.y -= 1
      case 1 => p.x += 1
      case 2 => p.y += 1
      case 3 => p.x -= 1
    }
  }

  private def startGame{
    DrawRoad()
    for (i <- 5 to 0 by -1){
      clients.foreach(p => {
        p.oos.writeObject(CountDown(i))
        p.oos.flush
      })
      Thread.sleep(1000)
    }

    clients.foreach(p => {
      p.oos.writeObject( GameDrawRoad(obstacles))
      p.oos.flush
    })

    val p1 = mutable.Buffer(clients(0).x -> clients(0).y)
    val p2 = mutable.Buffer(clients(1).x -> clients(1).y)
    //ultimo
    //val p2Map = mutable.Buffer(clients(1).x2 -> clients(1).y2)
    val board = Array.fill(500, 500)(false)
    println("look bitch", obstacles(0).getBounds2D, "another", obstacles(0).getCenterX )
    var checkerP1 = 0
    var checkerP2 = 0
    while (winner < 0){
      // readPlayers
      clients.foreach(move)
      p1 += clients(0).x -> clients(0).y
      p2 += clients(1).x -> clients(1).y
      checkerP1 = checkPos(clients(0).x)
      checkerP2 = checkPos(clients(1).x)
      if ( lose(0) || collide(obstacles(checkerP1),0) ) winner = 0
      //debo pensar en la forma de que este mirando la ubicacion y que si toca uno de los rectangulos inmediatemente pierda
      else if (lose(1) || collide(obstacles(checkerP2),1)) winner = 1
      board(clients(0).x)(clients(0).y) = true
      board(clients(1).x)(clients(1).y) = true
      clients.foreach(p => {
        p.oos.reset
        p.oos.writeObject(StepTaken(p1,p2))
        p.oos.flush
      })
      Thread.sleep(50)
    }

    clients.foreach(p => {
      p.oos.writeObject(GameEnds(winner))
      p.oos.flush
    })
  }
}