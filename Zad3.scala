/*
  Zadanie 3.

  Korzystając z aktorów zaimplementuj „symulator konkursu”. Jego infrastruktura
  powinna składać się z aktora głównego, typu Organizator

    class Organizator extends Actor { ... }

  Początkowo Organizator powinien być w stanie przyjąć jedynie komunikat

    case class Init(liczbaEgzaminatorów: Int)

  w reakcji na który tworzy zadaną liczbę aktorów typu Egzamin

    class Egzamin extends Actor { ... }

  Następnie przechodzi do stanu w którym będzie gotowy przyjąć komunikat

    case object Rozpocznij

  Po jego otrzymaniu, Nadzorca powinien wysłać do wszystkimi egzaminatorów, komunikat

    case class Przepytaj(n: Int)

  gdzie n jest losowo wygenerowaną liczbą z zakresu od 1 do 100. Po otrzymaniu tego
  komunikatu każdy Egzaminator powinien utworzyć, zadaną przez „n” liczbę aktorów typu

    class Uczestnik extends Actor { ... }

  oraz do każdego z nich wysłać komunikat typu

    case object Pytanie

  Po otrzymaniu pytania Uczestnicy powinni odesłać Egzaminatorowi odpowiedź w postaci komunikatu

    case class Odpowiedź(seq: Seq[Int])

  Gdzie seq jest losowo wygenerowaną sekwencją zawierającą 10 elementów w postaci: 0 lub 1,
  np. Seq(0, 1, 0, 0, 1, 1, 0, 1, 1, 1), gdzie 0 oznacza złą odpowiedź, a 1 dobrą.

  Egzaminator powinien sumować wszystkie wyniki oraz po otrzymaniu odpowiedzi od wszystkich
  uczestników przesłać informację o uczestnikach z najlepszym wynikiem do Organizatora, używając
  komunikatu

    case class Wybór(/* wymyśl atrybuty */)

  przy pomocy, którego przekaże zbiór najlepszych uczestników (może być kilku uczestników, którzy
  uzyskali najlepszy wynik), oraz ich wynik, Dodatkowo powinien też wysłać komunikat

    case object Zakończ

  do Uczestników, którzy nie uzyskali najlepszego wyniku. Po jego otrzymaniu powinni oni zakończyć
  swoje działanie.

  Po otrzymaniu danych od wszystkich egzaminatorów Organizator zatrzymuje egzaminatorów (używając PoisonPill),
  a następnie wybiera zwycięzców konkursu. Są nimi kandydaci, którzy uzyskali najlepszy wynik (może być ich kilku)
  oraz wypisuje na konsoli informację odnośnie tego, którzy kandydaci wygrali. Następnie zamyka system aktorów,
  kończąc działanie całego programu.

  UWAGA!!! W swoim rozwiązaniu nie używaj zmiennych, kolekcji mutowalnych, ani pętli (konstrukcji takich, jak
  „while” czy „foreach”).

  Wartość zadania: 4 pkt.

*/
import akka.actor._

import scala.io.StdIn
import scala.util.Random
import scala.util.control.Breaks.{break, breakable}
object Utl{
  val rand: Random.type = scala.util.Random
}
object Zad3 {

  case class Init(liczbaEgzaminatorów: Int)

  case object Rozpocznij

  case class Przepytaj(n: Int)

  case object Pytanie

  case class Odpowiedź(seq: Seq[Int])

  case class Wybór(selected: Map[ActorRef, Seq[Int]])

  case object Zakończ

  class Organizator extends Actor {
    def receive: Receive = {
      case Init(n: Int) =>
        val egzaminatorzy: List[ActorRef] = (for {
          i <- 1 to n
        } yield context.actorOf(Props[Egzaminator](), s"egz$i")).toList
        context.become(rozpoczecie(egzaminatorzy))
    }

    def rozpoczecie(egzaminatorzy: List[ActorRef]): Receive = {
      case Rozpocznij =>

        for {
          i <- egzaminatorzy.indices
        } yield egzaminatorzy(i) ! Przepytaj(Utl.rand.nextInt(100) + 1)
        context.become(sprawdzenie(Map[ActorRef, Seq[Int]](), egzaminatorzy.length, egzaminatorzy))
      case Wybór(selected: Map[ActorRef, Seq[Int]]) =>
        context.become(sprawdzenie(selected, egzaminatorzy.length - 1, egzaminatorzy))
    }

    def sprawdzenie(oceny: Map[ActorRef, Seq[Int]], ilu: Int, egzaminatorzy: List[ActorRef]): Receive = {

      case Wybór(selected: Map[ActorRef, Seq[Int]]) =>
        if (ilu == 1) {
          val wszystkieOceny = oceny ++ selected
          val ocenyx: Seq[(ActorRef, Int)] = wszystkieOceny.toSeq.map(x => (x._1, x._2.sum))
          val best: Seq[ActorRef] = ocenyx.filter(a => a._2 == ocenyx.map(a => a._2).max).map(a => a._1)
          println(wszystkieOceny.toSeq.filter(a => best.contains(a._1)).map(a => (a._1.path.name, a._2)))
          self ! Zakończ
        }
        context.become(sprawdzenie(oceny ++ selected, ilu - 1, egzaminatorzy))
      case Zakończ =>
        for {
          i <- egzaminatorzy.indices
        } yield egzaminatorzy(i) ! PoisonPill
        context.system.terminate()
    }
  }

  class Egzaminator extends Actor {
    def receive: Receive = {
      case Przepytaj(n: Int) =>
        for {
          i <- 1 to n
        } yield context.actorOf(Props[Uczestnik](), s"${self.path.name}Uczestnik$i") ! Pytanie
        context.become(odbierzWyniki(n, Map[ActorRef, Seq[Int]](), sender()))
    }

    def odbierzWyniki(liczbaOdpowiedzi: Int, wyniki: Map[ActorRef, Seq[Int]], organizator: ActorRef): Receive = {
      case Odpowiedź(wynik: Seq[Int]) =>
        if (liczbaOdpowiedzi == 1) {
          val xwyniki = wyniki ++ Map(sender() -> wynik)
          val najlepsi: Map[ActorRef, Seq[Int]] = Map(xwyniki
            .toSeq
            .sortBy(_._2.sum).take(20).reverse: _*)
          organizator ! Wybór(najlepsi)
          val pozostali: Seq[ActorRef] = xwyniki
            .toSeq
            .diff(najlepsi.toSeq)
            .map(a => a._1)
          for {
            i <- pozostali.indices
          } pozostali(i) ! PoisonPill

        }
        else {
          context.become(odbierzWyniki(liczbaOdpowiedzi - 1, wyniki ++ Map(sender() -> wynik), organizator))
        }
    }
  }

  class Uczestnik extends Actor {
    def receive: Receive = {
      case Pytanie =>
        val odp: Seq[Int] = (for {
          i <- 1 to 10
        } yield Utl.rand.nextInt(2)).toSeq
        sender() ! Odpowiedź(odp)
    }
  }

  def main(args: Array[String]): Unit = {

    // r jest „generatorem”, którego należy użyć do generowania wartości
    // losowych różnych typów (i zakresów) np. r.nextInt, r.nextInt(100)

    val sys = ActorSystem("egz")
    val organizator = sys.actorOf(Props[Organizator](), "organizator")


    breakable {
      while (true) {
        StdIn.readLine("polecenie: ") match {
          case "start" =>
            organizator ! Init(1)
          case "rozpocznij" =>
            organizator ! Rozpocznij

          case _ =>
            break()
        }
      }
    }
  }
}
