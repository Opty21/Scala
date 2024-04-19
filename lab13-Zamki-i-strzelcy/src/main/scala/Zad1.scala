import akka.actor.{ActorSystem, Actor, Props, ActorRef, PoisonPill, Terminated}
import scala.concurrent.duration._
/*
Stwórz symulację bitwy dwóch rodów używając aktorów. Każdy ród posiada Zamek i 500 Łuczników. Żeby walka była uczciwa pojedynczy Planista (Scheduler) wydaje obu Zamkom rozkaz strzelania, który rozsyłany jest wśród Łuczników, którzy w danej chwili bronią murów. Zasady walki stanowią:
Zamki zaczynają ze 100 Łucznikami, których nazwiemy "aktywnych obrońców";
W każdej chwili Zamku może bronić maksymalnie 100 aktywnych obrońców;
Scheduler wysyła obu Zamkom rozkaz strzelania co 1 sekundę;
Strzały trafiają w Zamek i spadają na jego Łuczników, mając szansę na trafienie równą ([liczba_aktywnych_obrońców]/(2 * 100)), np:
gdy wrogiZamek ma 100 aktywnych Łuczników, nasz strzał ma 50% szansy na trafienie;
gdy wrogiZamek ma 50 aktywnych Łuczników, nasz strzał ma 25% szansy na trafienie.
Obrońca traci życie gdy otrzyma postrzał;
Zamek uzupełnia swoich obrońców do momentu wyczerpania wszystkich rezerwowych Łuczników;
Żeby być zdolnym do walki Zamek musi mieć przynajmniej jednego aktywnego Łucznika;
Gdy wszyscy Łucznicy z danego Zamku zginą, ogłasza on, że przegrał bitwę – kończy to symulację (system.terminate).
Uwagi do rozważenia:
To, że strzelanie odbywa się co 1 sekundę nie jest ważne – może to być 0.01 sekundy żeby bitwa trwała krócej. Ważne żeby każdy Łucznik miał taką samą szybkość strzału.
Możliwe są różne strategie uzupełniania poległych obrońców. Pytanie, czy lepiej częściej uzupełniać Łuczników, aby mieć ich jak najwięcej czy trzymać się mniejszej liczby – aby strzały przeciwnika częściej pudłowały?
Uwagi techniczne:
Zaimplementuj symulację i wymyśl/przetestuj przynajmniej dwie różne strategie uzupełniania Łuczników. Przeprowadzając kilka symulacji, sprawdź która najlepiej się sprawdza.
W celu poinformowania Zamku, że jego obrońca ginie, z poziomu Zamku, wykorzystaj metodę context.watch(obrońca). Wówczas w przypadku śmierci obrońcy Zamek automatycznie otrzyma komunikat Terminated(obrońca), na który powinien sensownie zareagować.
*/

object Zad1 {
  // liczba lucznikow, liczba aktywnych
  val totalZaloga = 500
  val startZaloga = 100
  // wiadomosci do lucznikow
  case object ObudzSie
  case class StrzelajWZamek(wrogiZamek: ActorRef)
  // wiadomości do zamków
  case object Trafiony
  case class PrzygotujZamek(wrogiZamek: ActorRef)
  object Strzelanie
  case class StrzalaWZamek(wrogiZamek: ActorRef)
  object PodajStan

  class Zamek extends Actor {
    def receive: Receive = {
      // zebranie załogi, wybranie aktywnych
      case PrzygotujZamek(wrogiZamek: ActorRef) =>
        val obroncy = (for {
            i <- Range(0, totalZaloga)
            } yield context.actorOf(Props[Lucznik](), s"Lucznik${i+1}"))
            .toList
        val (aktywni, rezerwowi) = obroncy.splitAt(startZaloga)
        aktywni.foreach(x => x ! ObudzSie)
        println(s"Zamek ${self.path.name}: gotowy")
        context.become(wojna(rezerwowi, aktywni, wrogiZamek))
    }
    def wojna(rezerwa: List[ActorRef], aktywni: List[ActorRef], wrogiZamek: ActorRef): Receive = {
      // przeprowadzanie ataków, monitorowanie strat
      case PodajStan =>
        println(s"zamek ${self.path.name} ma jeszcze ${aktywni.size + rezerwa.size} lucznikow")
      case Strzelanie => 
        aktywni.foreach(x => x ! StrzelajWZamek(wrogiZamek))
        // context.watch pozwoli zauważyć śmierć łucznika
        aktywni.foreach(x => context.watch(x))
      // lucznik przeciwnika strzelił w nas
      case StrzalaWZamek => 
        // zamek decyduje czy strzała trafia
        val r = scala.util.Random
        val prawdopodobienstwo = aktywni.size/(2*100) * 100
        if (r.nextInt(100) <= prawdopodobienstwo) {
          // zawsze pierwszego z brzegu
          aktywni.head ! Trafiony
          context.become(wojna(rezerwa, aktywni.tail, wrogiZamek))
        }   
      case Terminated(obronca) =>
        // val brakuje = startZaloga - aktywni.size
        val brakuje = (startZaloga - aktywni.size)/2
        // w losowej zbyt często dobieraliśmy za malo obroncow, co ma tragiczne skutki kiedy jest ich np. 2
        // val brakuje = scala.util.Random.between(aktywni.size, 100) - aktywni.size
        if (rezerwa.size > 0 ) {
          if (rezerwa.size > brakuje) {
            rezerwa.take(brakuje).foreach(x => x ! ObudzSie)
            rezerwa.take(brakuje).foreach(x => context.watch(x))
            val nowiAktywniObroncy: List[ActorRef] = aktywni ++ rezerwa.take(brakuje)
            context.become(wojna(rezerwa.drop(brakuje), nowiAktywniObroncy, wrogiZamek))
          }
          else {
            println(s"w zamku ${self.path.name} skonczyla sie zaloga rezerwowa")
            rezerwa.foreach(x => x ! ObudzSie)
            rezerwa.foreach(x => context.watch(x))
            val nowiAktywniObroncy: List[ActorRef] = aktywni ++ rezerwa
            context.become(wojna(List[ActorRef](), nowiAktywniObroncy, wrogiZamek))
          }
        }
        else if (rezerwa.size == 0 && aktywni.size == 0) {
          println(s"zamek ${self.path.name} wyeksterminowany")
          // każemy wrogowi powiedzieć jaką zachował przewagę
          wrogiZamek ! PodajStan
          context.system.terminate()
        }
    }
  }

  class Lucznik extends Actor {
    def receive: Receive = {
      case ObudzSie => context.become(aktywny)
    }
    def aktywny: Receive = {
      case StrzelajWZamek(wrogiZamek) => 
        wrogiZamek ! StrzalaWZamek
      case Trafiony => {
        println(s"tu ${self.path.name} z zamku ${sender.path.name}: wlasnie umieram")
        self ! PoisonPill
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("system")
    import system.dispatcher
    val zamek1 = system.actorOf(Props[Zamek](), "za_gorami")
    val zamek2 = system.actorOf(Props[Zamek](), "za_lasami")
    zamek1 ! PrzygotujZamek(zamek2)
    zamek2 ! PrzygotujZamek(zamek1)

    val ticker = system.scheduler.scheduleWithFixedDelay(
      Duration.Zero,
      100.milliseconds,
      zamek1,
      Strzelanie
    )
    val ticker2 = system.scheduler.scheduleWithFixedDelay(
      Duration.Zero,
      100.milliseconds,
      zamek2,
      Strzelanie
    )
  }
}
