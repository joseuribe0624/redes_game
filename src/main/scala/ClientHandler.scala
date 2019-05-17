import java.io.{ObjectInputStream, ObjectOutputStream}

import Server.{turnLeft, turnRight}

class ClientHandler(var winner: Int, ois: ObjectInputStream , oos: ObjectOutputStream) extends Thread{
  def set_winner(new_winner: Int): Unit ={
    winner = new_winner
  }

  override def run(): Unit = {
    while (winner < 0) {
      ois.readObject() match {
        case Client.TurnLeft(p) =>
          turnLeft(p)
        case Client.TurnRight(p) =>
          turnRight(p)
      }
    }
  }
}
