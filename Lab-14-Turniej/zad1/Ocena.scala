package zad1

case class Ocena(nota1: Int, nota2: Int, nota3: Int) extends Ordered[Ocena]{
  override def compare(that: Ocena): Int ={
    val suma1 = nota1 + nota2 + nota3
    val suma2 = that.nota1 + that.nota2 + that.nota3
    val diff = suma1 - suma2
    if((suma1 - suma2) == 0) return nota1 - that.nota1
    diff
  }
}