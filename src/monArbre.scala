import scala.math._

object monArbre {
  
  
  def error(message: String):Nothing ={
     println(message)
     throw new Exception
  }
  
  //Main
  def main(args:Array[String]):Unit={
    val a=12
    val a1 = Noeud(12, ArbreVide, ArbreVide)
    println("Coucou ;) ")
  }
  
  def memeforme1(a1:Arbre ,a2:Arbre):Boolean= (a1, a2) match {
    case (ArbreVide, ArbreVide) => true
    case (ArbreVide, _) => false
    case ( _, ArbreVide) => false
    case (Noeud(_,fg1,fd1),Noeud(_,fg2,fd2)) => memeforme1(fg1,fd1) && memeforme1(fg2,fd2)
  }
  
  def foisn(n:Int, a:Arbre ):Arbre=a match{
    case ArbreVide => ArbreVide
    case Noeud(valeur:Int , fg, fd) => Noeud(valeur *n, foisn(n,a), foisn(n, a)) 
  }
  
  
  trait Arbre {
    
    def fd: Arbre
    def fg: Arbre
    
    def memeforme(a:Arbre):Boolean= (this, a) match {
      case (ArbreVide, ArbreVide) => true
      case (ArbreVide, _) => false
      case ( _, ArbreVide) => false
      case (Noeud(_,fg1,fd1),Noeud(_,fg2,fd2)) => fg1.memeforme(fg2) && fd1.memeforme(fd2)
    }
    
    def foisn(n:Double):Arbre= this match{
      case ArbreVide => ArbreVide
      case Noeud(valeur:Int , fg, fd) => Noeud(valeur *n, foisn(n), foisn(n)) 
      case Noeud(valeur:Double , fg, fd) => Noeud(valeur *n, foisn(n), foisn(n))
      case Noeud(valeur:Char , fg, fd) => Noeud(valeur , foisn(n), foisn(n))
    }
    
    def max(a:Int, b:Int):Int ={
      if(a>b)
        a
      else
        b
    }
    
    def hauteur():Int= this match{
      case ArbreVide => 0
      case Noeud(_,fg,fd) => 1+ max(fg.hauteur(), fd.hauteur()) 
    }
    
    def contient[A](n:A, a:Arbre ):Boolean= this match{
      case ArbreVide => false       
      case Noeud(valeur:A, fg, fd) => valeur == n ||fg.contient(n, a) || fd.contient(n, a)
    }
    
    def map[A](f:Double=>A): Arbre = this match {
      case ArbreVide => ArbreVide
      case Noeud(v:Int, fg, fd) => Noeud(f(v), fg.map(f), fd.map(f))
      case Noeud(v:Double, fg, fd) => Noeud(f(v), fg.map(f), fd.map(f))
      case _ => this
    }
    
    def miroir(): Arbre = this match {
      case ArbreVide   => ArbreVide
      case Noeud(v, fg, fd) => Noeud(v, fd.miroir, fg.miroir)
    }
    
    
    
    def toList(ordre:Int):List[String] = {
        if (ordre < -1 || ordre > 1)
        Nil
        else
        (this, ordre) match {
        case (ArbreVide, _)      => Nil
        case (Noeud(v, fg, fd), -1) => v.toString::fg.toList(ordre):::fd.toList(ordre)
        case (Noeud(v, fg, fd), 0) => fg.toList(ordre) ::: v.toString::fd.toList(ordre)
        case (Noeud(v, fg, fd), 1) => fg.toList(ordre) ::: fd.toList(ordre) ::: List(v.toString)
        }
   }
    
  }
  
  case object ArbreVide extends Arbre {
    def fd = error("Opération non définie")
    def fg = error("Opération non définie")
  }
  
  case class Noeud[A](info:A, fg:Arbre, fd:Arbre) extends Arbre 
  //finObjet
}