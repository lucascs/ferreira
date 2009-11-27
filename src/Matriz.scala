class Matriz[T](linhas:Int, colunas:Int) {
  val matriz:Array[Array[T]] = {
    val result = new Array[Array[T]](linhas)
    (0 to colunas - 1).foreach(result(_) = new Array(colunas))
    result
  }
  def apply(i:Int, j:Int):T = matriz(i)(j)
  
  def apply(i:Int):Array[T] = matriz(i)
  
  def update(i:Int, j:Int, valor:T):Unit = {
    matriz(i)(j) = valor
  }
}
