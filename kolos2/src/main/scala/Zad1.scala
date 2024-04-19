import akka.actor._

object Zad1 {

  case object Przygotuj
  case class Przyjmij(lTren: List[ActorRef])
  case object Rozpocznij
  case class Rozgrywka(t1: ActorRef, t2: ActorRef)
  case object Wybierz
  case class Wybor(lZaw: List[ActorRef])
  case class Nowy(zaw: ActorRef)
  case class Koniec(zwTrener: ActorRef)
  
  case object Gra
  case class Winny(zaw: ActorRef)

  class Organizator extends Actor {
    def receive: Receive = {
      case  Przyjmij(lTren: List[ActorRef]) =>
        val sedziowie: List[ActorRef] = (for {
          i <- 1 to lTren.length/2
      } yield context.system.actorOf(Props[Trener]())).toList
      context.become(czekanieNaStart(listaSedz,lTren,List[ActorRef](),listaSedz.length))
    }
    def czekanieNaStart(listaSedz: List[ActorRef],listaTren: List[ActorRef],tempTren: List[ActorRef],pozostaleRundy: Int): Receive = {
      case Rozpocznij =>
      println(listaTren)
      val temp = Random.shuffle(listaTren).sliding(2,2)
      println(temp)
      for(i <- 0 to listaSedz.length){
        listaSedz(i) ! Rozgrywka(temp(i)(0),temp(i)(1))
      }
      case Koniec(zwTrener: ActorRef) =>
        if(pozostaleRundy == 1){
          val dropCount = listaSedz.length/2
          for(i <- 0 to dropCount){
            listaSedz(i) ! PoisonPill
          }
          val tempList = listaSedz.drop(dropCount)
          context.become(czekanieNaStart(tempList,zwTrener :: tempTren,List[ActorRef](),tempList.length))
          self ! Rozpocznij
        } else {
          context.become(czekanieNaStart(listaSedz,listaTren,zwTrener :: tempTren,pozostaleRundy - 1))
        }
    }
  }



  class Trener extends Actor {
    def receive: Receive = {
      case Przygotuj =>
        val zawodnicy: List[ActorRef] = (for {
          i <- 1 to r.nextInt(100)+7
      } yield context.system.actorOf(Props[Zawodnik]())).toList
      context.become(czekanieNaStart(zawodnicy))
    }
    def czekanieNaStart(zawodnicy: List[ActorRef]):Receive = {
      case Wybierz => 
        sender() ! Wybor(zawodnicy.take(7))
        context.become(start(zawodnicy.drop(7)))
    }
    def start(zawodnicy: List[ActorRef]): Receive = {
      case Winny(zaw: ActorRef) =>
        zaw ! PoisonPill
        if(zawodnicy.length > 0){
          sender() ! Nowy(zawodnicy.take(1))
          context.become(start(zawodnicy.drop(1)))
        } else {
          context.stop()
        }

    }
  }

  class Zawodnik extends Actor {
    def receive: Receive = {
      case msg => println("x")
    }
  }
  
  class Sedzia extends Actor {
    def receive: Receive = {
      case Rozgrywka(org: ActorRef,t1: ActorRef, t2: ActorRef) =>
        context.become(rozpocznijRozgrywke(org,t1,t2))
        context.watch(t1)
        context.watch(t2)
        t1 ! Wybierz
        t2 ! Wybierz
    }
    def rozpocznijRozgrywke(org: ActorRef,t1: ActorRef, t2: ActorRef,t1Team: List[ActorRef],t2Team: List[ActorRef]): Receive = {
      case Wybor(lZaw: List[ActorRef]) =>
        if(sender() == t1){
          if(t2Team.length > 0){
            context.become(rozgrywkaTrwa(org,t1,t2,lZaw,t2Team,0,0))
            self ! Gra
          } else {
          context.become(rozpocznijRozgrywke(org,t1,t2,lZaw,t2Team))
          }
        } else{
          if(t1Team.length > 0){
            context.become(rozgrywkaTrwa(org,t1,t2,t1Team,lZaw,0,0))
            self ! Gra
          }else {
          context.become(rozpocznijRozgrywke(org,t1,t2,t1Team,lZaw))
          }
        }
      
    }
    def rozgrywkaTrwa(org: ActorRef,t1: ActorRef, t2: ActorRef,t1Team: List[ActorRef],t2Team: List[ActorRef],t1Points:Int,t2Points:Int): Receive = {
      case Gra =>
        val r = util.Random
        val temp = r.nextFloat()
        if(temp < 0.5) {
          val randomShuffle = Random.shuffle(t1Team)
          val winny = randomShuffle.take(1)
          t1 ! Winny(winny)
          context.become(rozgrywkaTrwa(org,t1,t2,randomShuffle.drop(1),t2Team,t1Points,t2Points+1))
          if(t2Points == 24){
            t1 ! PoisonPill

          }
        } else {
          val randomShuffle = Random.shuffle(t2Team)
          val winny = randomShuffle.take(1)
          t2 ! Winny(winny)
          context.become(rozgrywkaTrwa(org,t1,t2,t1Team,randomShuffle.drop(1),t1Points+1,t2Points))
          if(t1Points == 24){
            t2 ! PoisonPill
          }
        }
      case Nowy(zaw: ActorRef) =>
        if(sender() == t1){
          context.become(rozgrywkaTrwa(org,t1,t2,zaw :: t1Team,t2Team,t1Points,t2Points))
        } else {
          context.become(rozgrywkaTrwa(org,t1,t2, t1Team,zaw ::t2Team,t1Points,t2Points))
        }
        self ! Gra
      case Terminated(trener: ActorRef) =>
        if(trener == t1){
          org ! Koniec(t2)
        } else {
          org ! Koniec(t1)
        }
        context.become()
    }
  }

  def main(args: Array[String]): Unit = {
    val r = util.Random
    val system = ActorSystem("egz")
    val org = system.actorOf(Props[Organizator], "org")
    val trenerzy: List[ActorRef] = (for {
          i <- 1 to r.nextInt(100)
      } yield context.system.actorOf(Props[Trener]())).toList
      org ! Przyjmij(trenerzy)
      org ! Rozpocznij
  }

}
