object C {

  def apply(im: Double) = new C(im, 0)

}

case class C(re : Double, im : Double){

  def this(re: Double) = {

    this(re, 0)

  }

  override def toString: String = {

    if (im > 0) s"$re+$im"

    else if (im < 0) s"$re$im"

    else s"$re"

  }

  def +(that: C): C = C(this.re + that.re, this.im + that.im)
  def +(that: Double): C = this + C(that)

  def -(that: C): C = C(this.re - that.re, this.im - that.im)
  def -(that: Double): C = this - C(that)

  def *(that: C): C = C(this.re * that.re - this.im * that.im, this.re * that.im + this.im * that.re)
  def *(that: Double): C = this * C(that)

  def /(that: C): C = {

    try {

      val div = that.re * that.re + that.im * that.im
      if (div == 0) throw new  IllegalArgumentException()
      C((this.re * that.re + this.im * that.im) / div, (this.im * that.re - this.re * that.im) / div)

    }

    catch {

      case _: IllegalArgumentException =>

        println("Cannot divide by 0")
        C(Double.NaN)

    }

  }

  def /(that: Double): C = this / C(that)

}

object RunMe extends App{

  println(C(2f, 3f))
  println(C(2f, -3f))
  println(C(2))
  println(C(2f,3f) + C(2f, 3f))
  println(C(2f,3f) - C(2f, 3f))
  println(C(2f,3f) / C(2f, 3f))
  println(C(2f,3f) / C(0))
  println(C(5) * 5)
  println(C(5,1) * 5)
  println(C(5) / 0)

}