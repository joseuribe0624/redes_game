import collection.mutable
import java.net._
import java.io._
import java.awt.geom.Rectangle2D

import scala.collection.mutable.ArrayBuffer



object Server {

  case class CountDown(value: Int)

  //agregue p1Map
  case class StepTaken(p1: Seq[(Int, Int)], p2: Seq[(Int, Int)], p3: Seq[(Int, Int)], p4: Seq[(Int, Int)])

  case class GameEnds(winner: Int)

  case class GameDrawRoad(rectangles: ArrayBuffer[Rectangle2D])

  var obstacles: ArrayBuffer[Rectangle2D] = new ArrayBuffer[Rectangle2D]
  var linesRoad: ArrayBuffer[Rectangle2D] = new ArrayBuffer[Rectangle2D]

  private class Player(var id: Int, var sock: Socket, var ois: ObjectInputStream, var oos: ObjectOutputStream,
               var x: Int, var y: Int, var dir: Int, var x2: Int, var y2: Int)
  private class Backup_Player(var id: Int, var x: Int, var y: Int, var dir: Int, var x2: Int, var y2: Int)


  case class PlayerS(playerNumber: Int, x: Int, y: Int, dir: Int, x2: Int, y2: Int)
  case class Clients(clients: mutable.ArrayBuffer[PlayerS])
  case class Slave(sock: Socket, oos: ObjectOutputStream)
  private val slaves = mutable.Buffer[Slave]()
  private var master: Helper.Server = Helper.Server(null)
  private val Me = Helper.Server(InetAddress.getByName("192.168.121.38"))

  val PORT_GAME: Int = 4450
  val PORT_DATA: Int = 4449
  val PORT_ALIVE: Int = 4447

  //map
  //class Maps(val sock: Socket, var x:Int, var y: Int)
  private val clients = mutable.Buffer[Player]()
  private val backup_clients = mutable.Buffer[Backup_Player]()
  //private val obstacles = mutable.Buffer[Maps]()
  private var winner = -1
  private var ss: ServerSocket = _

  private val start_pos_y = Array(50, 100, 150, 200)

  def main(args: Array[String]){
    new ServerSocket(PORT_ALIVE)
    receive_slaves()
    ss = new ServerSocket(PORT_GAME)

    var loops = 54

    while (loops != 0) {
      master = Helper.getMasterAvailableServer()
      println(master)

      try {
        master match {
          case Me => lodge()
          case Helper.Server(null) =>
          case _ => listen_to_master()
        }
      } catch {
        case _: Throwable =>
          println("Something was wrong with Master, Choosing a new master...")
      }

      loops -= 1
    }
  }


  def listen_to_master(): Unit = {
    println("I am a slave")
    val socket = new Socket()
    socket.connect(new InetSocketAddress(master.ip, PORT_DATA), 900)

    val ois = new ObjectInputStream(new BufferedInputStream(socket.getInputStream()))

    val read_data: Runnable = new Runnable {
      override def run(): Unit = {
        while (socket.isConnected) {
          val t = ois.readObject()
          t match {
            case Clients(backup) =>
              backup.foreach(c => {
                if (c.playerNumber >= backup_clients.length) backup_clients += new Backup_Player(c.playerNumber, 0, 0, 0, 0, 0)
                backup_clients(c.playerNumber).x = c.x
                backup_clients(c.playerNumber).y = c.y
                backup_clients(c.playerNumber).dir = c.dir
                backup_clients(c.playerNumber).x2 = c.x2
                backup_clients(c.playerNumber).y2 = c.y2
              })
          }
        }
      }
    }

    val t = new Thread(read_data)
    t.start()
    var is_reachable = true
    while (is_reachable) is_reachable = master.ip.isReachable(5)
    throw new SocketException("Unreachable")

  }

  val update_slaves: Runnable = new Runnable {
    override def run(): Unit = {
      slaves.foreach(s => {
        val backup = mutable.ArrayBuffer[PlayerS]()
        clients.foreach(p => {
          backup.append(PlayerS(p.id, p.x, p.y, p.dir, p.x2, p.y2))
        })
        try {
          s.oos.writeObject(Clients(backup))
          s.oos.flush()
        } catch {
          case _: SocketException =>
        }
      })
    }
  }

  def receive_slaves(): Unit = {
    val receive_slaves: Runnable = new Runnable  {
      val so = new ServerSocket(PORT_DATA)
      override def run(): Unit = {
        while (!so.isClosed) {
          val sock = so.accept
          val some = Helper.servers.find((x: Helper.Server) => {
            val ip = sock.getRemoteSocketAddress.asInstanceOf[InetSocketAddress]
            ip.getHostString == x.ip.getHostAddress
          })

          if (some.nonEmpty) {
            println("New slave connected")
            connect_server(sock)
          }
        }
      }
    }

    val t: Thread = new Thread(receive_slaves)
    t.start()
  }


  def lodge(): Unit = {
    println("I am master")
    while (clients.lengthCompare(4) < 0){
      val sock = ss.accept
      connect_client(sock)
    }

    startGame()
  }

  def connect_server(sock: Socket): Unit = {
    val oos = new ObjectOutputStream(new BufferedOutputStream(sock.getOutputStream()))
    oos.flush()
    slaves.append(Slave(sock, oos))
  }

  def connect_client(sock: Socket){
    println("New client connected")
    val oos = new ObjectOutputStream(new BufferedOutputStream(sock.getOutputStream()))
    oos.flush()
    val ois = new ObjectInputStream(new BufferedInputStream(sock.getInputStream()))

    val id = ois.readInt()
    if (id < 0) {
      clients += new Player(clients.length, sock, ois, oos, 10, start_pos_y(clients.length), 1, 70 , start_pos_y(clients.length) )
    } else {
      val player = new Player(id, sock, ois, oos, backup_clients(id).x, backup_clients(id).y, backup_clients(id).dir, backup_clients(id).x2, backup_clients(id).y2)
      if (id >= clients.length ) clients.append(player)
      else clients.insert(id, player)

    }

    val T = new ClientHandler(winner, ois, oos)
    T.start()
    if (id < 0) oos.writeObject(Client.InitPlayer(clients.length - 1))
    oos.flush()
  }

  def readPlayers(): Unit ={
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
        y = 50
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
    if (clients(playerNumber).x == 400 || clients(playerNumber).y == clients(playerNumber).y2 + 20 || clients(playerNumber).y == clients(playerNumber).y2-20){
      true
    }
    else{
      false
    }
  }

  def checkPos(actualPos: Int, jugador:Int): Int ={
    val check = 30
    if (actualPos < check + 150 ){
      if (jugador == 0){
        0
      }
      else if (jugador == 1) {
        1
      }
      else if (jugador == 2){
        2
      }
      else {
        3
      }
    }
    else if(actualPos < check + 150*2){
      if (jugador == 0){
        4
      }
      else if (jugador == 1) {
        5
      }
      else if (jugador == 2){
        6
      }
      else {
        7
      }
    }
    else{
      if (jugador == 0){
        8
      }
      else if (jugador == 1) {
        9
      }
      else if (jugador == 2){
        10
      }
      else {
        11
      }
    }
  }

  def collide(rect: Rectangle2D, playerNumber:Int): Boolean ={
    if(rect.getX < clients(playerNumber).x && clients(playerNumber).x < rect.getX + rect.getWidth
      && rect.getY < clients(playerNumber).y && clients(playerNumber).y < rect.getY + rect.getHeight ){
      true
    }
    else{
      false
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

  private def startGame(): Unit ={
    DrawRoad()
    for (i <- 5 to 0 by -1){
      clients.foreach(p => {
        p.oos.writeObject(CountDown(i))
        p.oos.flush()
      })
      Thread.sleep(1000)
    }

    clients.foreach(p => {
      p.oos.writeObject( GameDrawRoad(obstacles))
      p.oos.flush()
    })

    val p1 = mutable.Buffer(clients.head.x -> clients.head.y)
    val p2 = mutable.Buffer(clients(1).x -> clients(1).y)
    val p3 = mutable.Buffer(clients(2).x -> clients(2).y)
    val p4 = mutable.Buffer(clients(3).x -> clients(3).y)
    //ultimo
    //val p2Map = mutable.Buffer(clients(1).x2 -> clients(1).y2)
    val board = Array.fill(500, 500)(false)
    var checkerP1 = 1
    var checkerP2 = 2
    var checkerP3 = 3
    var checkerP4 = 4
    while (winner < 0){
      // readPlayers
      clients.foreach(move)
      p1 += clients.head.x -> clients.head.y
      p2 += clients(1).x -> clients(1).y
      p3 += clients(2).x -> clients(2).y
      p4 += clients(3).x -> clients(3).y
      checkerP1 = checkPos(clients.head.x, 0)
      checkerP2 = checkPos(clients(1).x,1)
      checkerP3 = checkPos(clients(2).x,2)
      checkerP4 = checkPos(clients(3).x,3)
      if ( lose(0) || collide(obstacles(checkerP1),0) ) winner = 0
      else if (lose(1) || collide(obstacles(checkerP2),1)) winner = 1
      else if (lose(2) || collide(obstacles(checkerP3), 2)) winner = 2
      else if (lose(3) || collide(obstacles(checkerP4),3)) winner = 3
      board(clients.head.x)(clients.head.y) = true
      board(clients(1).x)(clients(1).y) = true
      board(clients(2).x)(clients(2).y) = true
      board(clients(3).x)(clients(3).y) = true


      clients.foreach(p => {
        p.oos.reset()
        p.oos.writeObject(StepTaken(p1,p2, p3, p4))
        p.oos.flush()
      })
      Thread.sleep(50)

      val T = new Thread(update_slaves)
      T.start()
    }

    clients.foreach(p => {
      p.oos.writeObject(GameEnds(winner))
      p.oos.flush()
    })
  }
}