import scala.concurrent.ExecutionContext
import scala.util.control.Breaks._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.io.StdIn
import akka.actor.{ActorSystem, Props}
import zad1._
import scala.collection.immutable.ListMap
import akka.actor.{Actor, ActorRef, Props}


object Zad1 extends App {
  val system = ActorSystem("system")
  val organizator = system.actorOf(Props[Organizator](), "organizator")
  implicit val ec: ExecutionContext = ExecutionContext.global
  breakable {
    while (true) {
      StdIn.readLine("polecenie: ") match {
        case "start" =>
          organizator ! Organizator.Start
        case "eliminacje" =>
          organizator ! Organizator.Runda
          println("rundy rozegrane")
        case "final" =>
          organizator ! Organizator.Runda
          println("rundy II rozegrane")
        case "wyniki" =>
          implicit val timeout = Timeout(5 seconds)
          val future = organizator ? Organizator.Wyniki
          val result = Await.result(future, timeout.duration).asInstanceOf[ListMap[ActorRef, Option[Ocena]]]
          val lista = result.map{ case (key, value) => println(key.path.name + " " + value) }
        case "koniec" =>
          organizator ! Organizator.Stop
          break()
        case _ =>
      }
    }
  }
}
