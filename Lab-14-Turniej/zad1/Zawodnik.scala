package zad1

import akka.actor.Actor

object Zawodnik {
  case object Próba
  // polecenie wykonania próby (kończy się zwróceniem Wyniku,
  // za pomocą komunikatu Grupa.Wynik)
}

class Zawodnik extends Actor {
  import Zawodnik._
  //val identity: Osoba = Utl.osoba()
  def receive: Receive = {
    case Próba =>
      sender() ! Grupa.Wynik(Utl.ocena())
  }
}
