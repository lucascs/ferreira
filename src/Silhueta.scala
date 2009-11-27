import scala.io.Source
import java.lang.System
import java.io.InputStream
import java.io.FileInputStream
import java.io.PrintStream
import java.io.OutputStream
import java.io.ByteArrayInputStream

object Silhueta {
  
	val NLins = 600                     // número de linhas da imagem
	val NCols = 800                     // número de colunas da imagem
	val BordaInf = NLins - 1            // borda inferior (última linha da imagem) 
	val MargemInf = 20                  // linhas do eixo base à borda inferior da imagem
	val Base = BordaInf - MargemInf     // linha do eixo base 
	val Branco = 15                     // valor de maxval
	val Cinza = 10                      // cor da silhueta preenchida
	val Preto = 0                       // cor do eixo base

  def main(args: Array[String]) : Unit = {
     val alg = if (args.size >= 1) parseAlg(args(0)) else algoritmo3(_)
     val entrada = if (args.size >= 2) new FileInputStream(args(1)) else System.in 
     val saida = if (args.size >= 3) new PrintStream(args(2)) else System.out
     val pgm = if (args.size >= 4) Some(new PrintStream(args(3))) else None
     
     val edifs = le(entrada)
     val result = alg(edifs)
     imprime(result , saida)
     geraPGM(result, pgm)
  }
  
  def geraPGM(result:List[ElemSilhueta], pgm:Option[PrintStream]):Unit = pgm match {
    case None => 
    case Some(pgmFile) =>
      
    val maxh = (0 /: result) { _ max _.h }
    val maxx = result.last.x
   
    val matriz = new Matriz[Int](maxx, maxh)
   
    def preenche(lista:List[ElemSilhueta]):Unit = lista match {
      case x::y::tail => preencheRetangulo(matriz, x.x, y.x, 0, x.h, Preto)
      					 preenche(y::tail)
      case _ =>
    
    preenche(result)
    
    pgmFile.println("P2")
    pgmFile.println(NCols + " " + NLins)
    
    val multx = NCols/maxx
    
    for(i <- 0 to maxh -1) {
      matriz(i).foreach((0 to multx).foreach(x => pgmFile.print(x))
      pgmFile.println()
    }
  }
  
  def preencheRetangulo(a:Matriz[Int], 
                        lin1:Int, lin2:Int,
  						col1:Int, col2:Int, k: Int):Unit = {
     for (i <- lin1 to lin2; j <- col1 to col2)
       a(i, j) = k
  }
  
  def parseAlg(alg:String): ((List[Edificio]) => List[ElemSilhueta]) = alg match {
    case "1" => algoritmo1(_)
    case "2" => algoritmo2(_)
    case "3" => algoritmo3(_)
    case "L" => silhuetaComFoldLeft(_)
    case "R" => silhuetaComFoldRight(_)
  }
  
  def imprime(lista:List[ElemSilhueta], saida: PrintStream):Unit = {
    saida.println(lista.size)
    lista.foreach(x => saida.println(x.x + " " + x.h))
    saida.println()
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
    for (i <- 1 to (edifs.size - 1))
      result = uniao(result, silhuetaDeEdificio(edifs(i)))
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
    case elem::Nil => silhuetaDeEdificio(elem)
    case list => 
    	val (esquerda, direita) = list splitAt (list.size/2)
    	uniao(algoritmo3(esquerda), algoritmo3(direita))
  }
  
  def silhuetaComFoldLeft(edifs: List[Edificio]): List[ElemSilhueta] = 
	(List[ElemSilhueta]() /: edifs){ (list, elem) => uniao(silhuetaDeEdificio(elem), list)}  
  
  def silhuetaComFoldRight(edifs: List[Edificio]): List[ElemSilhueta] = 
  	(edifs :\ List[ElemSilhueta]()){ (elem, list) => uniao(silhuetaDeEdificio(elem), list)}  
   
  def uniao(s1: List[ElemSilhueta], s2: List[ElemSilhueta]): List[ElemSilhueta] = {
    def loop(xs: List[ElemSilhueta], ys: List[ElemSilhueta], hx: Int, hy:Int, hmax:Int): List[ElemSilhueta] = (xs, ys) match {
      case (Nil, x) => x
      case (x, Nil) => x
      
      case (e::es, d::ds) if e.x == d.x => {
    	val maisAlto = if (d.h > e.h) d else e
        if (maisAlto.h > hmax) 
          maisAlto :: loop(es, ds, e.h, d.h, maisAlto.h) 
        else 
          loop(es, ds, e.h, d.h, hmax)
      }
        
      case (e::es, d::ds) if e.x < d.x => 
        if (e.h > hx && e.h > hmax) //subindo
          e :: loop(es, d::ds, e.h, hy, e.h) 
        else if (hx == hmax) // descendo e o e é maior
          ElemSilhueta(e.x, hy max e.h) :: loop(es, d::ds, e.h, hy, hy)
        else
          loop(es, d::ds, e.h, hy, hmax)
      
      case (e::es, d::ds) if e.x > d.x => 
        if (d.h > hy && d.h > hmax) //subindo
          d :: loop(e::es, ds, hx, d.h, d.h) 
        else if (hy == hmax) // descendo e o d é maior
          ElemSilhueta(d.x, hx max d.h) :: loop(e::es, ds, hx, d.h, hx)
        else
          loop(e::es, ds, hx, d.h, hmax)
      
    }
	loop(s1, s2, 0, 0, 0)
  }
  
  def silhuetaDeEdificio(edif: Edificio): List[ElemSilhueta] = 
  	List(ElemSilhueta(edif.esq, edif.alt), ElemSilhueta(edif.dir, 0))

}
