import scala.io.Source
import java.io.InputStream
import java.io.ByteArrayInputStream

object Silhueta {
  
  def main(args : Array[String]) : Unit = {
     val entrada = """8
     |12 7 16
     |2 6 7
     |1 11 5
     |24 4 28 
     |3 13 9
     |19 18 22
     |23 13 29
     |14 3 25""".stripMargin
     val edifs = le(new ByteArrayInputStream(entrada.getBytes))
     println(edifs.size)
     val result = algoritmo1(edifs)
     imprime(result)
  }
  
  def imprime(lista:List[ElemSilhueta]):Unit = {
    println(lista.size)
    println(lista)
  }
  
  def le(stream:InputStream):List[Edificio] = {
    val linhas = Source.fromInputStream(stream).getLines;
    val n =  linhas.next.stripLineEnd.toInt
    linhas.take(n).map((linha) => {
    	var args = linha.stripLineEnd.split(" ")
    	Edificio(args(0).toInt, args(1).toInt, args(2).toInt)
    }).toList
  }
  
  def algoritmo1(edifs: List[Edificio]): List[ElemSilhueta] = {
    var result = silhuetaDeEdificio(edifs(0))
    for (i <- 1 to edifs.size)
      result = uniao(result, silhuetaDeEdificio(edifs(1)))
    result
  }
    
  def algoritmo2(edifs: List[Edificio]): List[ElemSilhueta] = {
	def loop(edificios: List[Edificio], acc: List[ElemSilhueta]): List[ElemSilhueta] = edificios match {
	    case Nil => acc
        case head::tail => loop(tail, uniao(silhuetaDeEdificio(head), acc)) 
	}
	loop(edifs, Nil)
  }
  
  def algoritmo3(edifs: List[Edificio]): List[ElemSilhueta] = edifs match {
    case Nil => Nil
    case list => 
    	val (esquerda, direita) = list splitAt list.size/2
    	uniao(algoritmo3(esquerda), algoritmo3(direita))
  }
  
  def silhuetaComFoldLeft(edifs: List[Edificio]): List[ElemSilhueta] = 
	(List[ElemSilhueta]() /: edifs){ (list, elem) => uniao(silhuetaDeEdificio(elem), list)}  
  
  def silhuetaComFoldRight(edifs: List[Edificio]): List[ElemSilhueta] = 
  	(edifs :\ List[ElemSilhueta]()){ (elem, list) => uniao(silhuetaDeEdificio(elem), list)}  
   
  def uniao(s1: List[ElemSilhueta], s2: List[ElemSilhueta]): List[ElemSilhueta] = {
    def loop(xs: List[ElemSilhueta], ys: List[ElemSilhueta], h: Int): List[ElemSilhueta] = (xs, ys) match {
      case (Nil, x) => x
      case (x, Nil) => x
      
      case (e::es, d::ds) if e.x == d.x => {
    	val maisAlto = if (d.h > e.h) d else e
        if (maisAlto.h > h) maisAlto :: loop(es, ds, maisAlto.h) else loop(es, ds, h)
      }
        
      case (e::es, d::ds) if e.x < d.x => if (e.h > h) e :: loop(es, d::ds, e.h) else loop(es, d::ds, h)  
      case (e::es, d::ds) if e.x > d.x => if (d.h > h) d :: loop(e::es, ds, d.h) else loop(e::es, ds, h)
      
    }
	loop(s1, s2, 0)
  }
  
  def silhuetaDeEdificio(edif: Edificio): List[ElemSilhueta] = 
  	List(ElemSilhueta(edif.esq, edif.alt), ElemSilhueta(edif.dir, 0))

}
