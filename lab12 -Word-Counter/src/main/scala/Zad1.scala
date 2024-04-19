object Zad1 {
  import akka.actor._
  
  class MyActor extends Actor {
    def receive: Receive = {
      case msg => println(s"Odebrałem wiadomość: $msg")
    }
  }
  object Nadzorca{
  case class Init(liczbaPracownikow: Int)
  case class Zlecenie(tekst: List[String])
  case class Wynik(ilosc: Int)
  case class Koniecd()
  } 
  object Pracownik{
    case class Wykonaj(tekst: String)
  }

  class Nadzorca extends Actor {
    import Nadzorca._
    import Pracownik._

    def receive: Receive = {
    case Init(n: Int) =>
      val pracownicy: List[ActorRef] = (for {
          i <- 1 to n
      } yield context.system.actorOf(Props[Pracownik]())).toList
      context.become(czekanie(pracownicy))
  }
    def czekanie(pracownicy: List[ActorRef]): Receive = {
      case Zlecenie(tekst: List[String]) =>
        context.become(working(pracownicy,pracownicy,tekst.length-1,0))
        self ! Zlecenie(tekst)
    }
    def Koniec(wynik: Int,pracownicy: List[ActorRef]): Receive = {
      case Koniecd() =>
        print("KONIEC: " + wynik + " xDDDDDDDDD")
        context.become(czekanie(pracownicy))
    }
    def working(pracownicy: List[ActorRef],wolniPracownicy: List[ActorRef],liniee: Int,wynik: Int): Receive = {
      case Zlecenie(tekst: List[String]) =>
        if(wolniPracownicy.length > 0 && liniee > 0){
          wolniPracownicy(0) ! Wykonaj(tekst(0))
          context.become(working(pracownicy,wolniPracownicy.tail,liniee-1,wynik))
          self ! Zlecenie(tekst.tail)
        } else if(wolniPracownicy.length == 0){
          context.become(working(pracownicy,wolniPracownicy,liniee,wynik))
          self ! Zlecenie(tekst)

        }
      case Koniecd() =>
        print("xD")
      case Wynik(ilosc: Int) => 
        if(liniee == 0 && wolniPracownicy.length == pracownicy.length-1){
          context.become(Koniec(wynik,pracownicy))
          self ! Koniecd()
        }
        else{
        context.become(working(pracownicy,sender() :: wolniPracownicy,liniee,wynik + ilosc))
        }
    }

  }

  class Pracownik extends Actor {
  import Pracownik._
  import Nadzorca._

  def receive: Receive = {
    case Wykonaj(linia: String) =>
      val ilosc = linia
        .toLowerCase()
        .split(" ")
        .length
      println(s"${self.path.name}: znalazlem ${ilosc} slow.")
      sender() ! Wynik(ilosc)
  }

}

  def main(args: Array[String]): Unit = {
    import Nadzorca._
    val dane = scala.io.Source.fromResource("ogniem_i_mieczem.txt")("UTF-8").getLines.toList
    
    val system = ActorSystem("WordCounter")
    val leonardo = system.actorOf(Props[Nadzorca], "leonardo")
    leonardo ! Init(2)
    leonardo ! Zlecenie(dane)
  }
}
