package zad1

import akka.actor.{Actor, ActorRef}

object Grupa {
  case object Runda
  case object Wyniki
  case class Wynik(ocena: Option[Ocena])
  case object Koniec
}
class Grupa(zawodnicy: List[ActorRef]) extends Actor {
  import Grupa._
  def receive: Receive = {
    case Runda =>
      import Zawodnik._
      for{
        i <- 0 to zawodnicy.length - 1
      } yield {
        zawodnicy(i) ! Próba
      }
    case Wynik(ocena: Option[Ocena]) =>
      context.become(zbieranieWynikow(Map[ActorRef, Option[Ocena]](sender() -> ocena), zawodnicy.length - 1))
    case msg => println(msg)
  }
  def zbieranieWynikow(refToMaybeOcena: Map[ActorRef, Option[Ocena]], i: Int): Receive = {
    case Wynik(ocena: Option[Ocena]) =>
      if(i == 1){
        context.become(zWynikami(refToMaybeOcena  ++ Map[ActorRef, Option[Ocena]](sender() -> ocena)))
      }
      else {
        context.become(zbieranieWynikow(refToMaybeOcena ++ Map[ActorRef, Option[Ocena]](sender() -> ocena), i - 1))
      }
    case _ =>
  }
  
  def zWynikami(refToMaybeOcena: Map[ActorRef, Option[Ocena]]): Receive ={
    case Wyniki => sender() ! refToMaybeOcena
  }
}
