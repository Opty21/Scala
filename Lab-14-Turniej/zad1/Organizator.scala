package zad1

import akka.actor.{Actor, ActorRef, Props}
import akka.util.Timeout
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import akka.pattern.ask
import scala.collection.immutable.ListMap

object Organizator {
  case object Start
  case object Runda
  case object Wyniki
  case class Wyniki(w: Map[ActorRef, Option[Ocena]])
  case object Stop
}

class Organizator extends Actor {
  import Organizator._
  def receive: Receive = {
    case Start =>
      println("zawody rozpoczęte")
      val zawodnicy: List[ActorRef] = (for {
        i <- 1 to 50
      } yield context.actorOf(Props[Zawodnik](), Utl.osoba())).toList
      val grupa: ActorRef = context.actorOf(Props(classOf[Grupa], zawodnicy))
      context.become(eliminacje(grupa))
    case Stop =>
      println("zawody przerwane")
      context.system.terminate()
  }
  def eliminacje(grupa: ActorRef): Receive ={
    case Runda =>
      grupa ! Grupa.Runda
    case Wyniki =>
      implicit val timeout = Timeout(3 seconds)
      val replyF = grupa ? Grupa.Wyniki
      val result = Await.result(replyF, timeout.duration).asInstanceOf[Map[ActorRef, Option[Ocena]]]
       // czekamy na odpowiedź z wynikami od grupy dzięki zapytaniu
      val posortowane = ListMap(result.toSeq.sortBy(_._2).reverse:_*)
      sender ! posortowane
      val finalisci: ActorRef = context.actorOf(Props(classOf[Grupa], posortowane.take(20).keys.toList))
      context.become(finals(finalisci))
    case Stop =>
      println("zawody przerwane")
      context.system.terminate()
    case _ =>
  }
  def finals(finalisci: ActorRef): Receive ={    
    case Runda =>
      finalisci ! Grupa.Runda
    case Wyniki =>
      implicit val timeout = Timeout(3 seconds)
      val reply = finalisci ? Grupa.Wyniki
      val result = Await.result(reply, timeout.duration).asInstanceOf[Map[ActorRef, Option[Ocena]]]
      val posortowane = ListMap(result.toSeq.sortBy(_._2).reverse:_*)
      sender ! posortowane
    case Stop =>
      println("konczymy zawody")
      context.system.terminate()
    case _ =>
  }
}
