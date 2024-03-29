package countdown

object solution {

  sealed abstract case class Expression {
    def valid:Boolean
	def apply:Int
  }
    
  case class Literal( x :Int ) extends Expression {
    def valid = x > 0
    def apply = x
  }
  
  case class Add(x :Expression, y :Expression) extends Expression {
    def valid = x.apply <= y.apply
    def apply=x.apply + y.apply
  }
  
  case class Mult(x :Expression, y :Expression) extends Expression {
    def valid = {
      val lhs = x.apply
      val rhs = y.apply
      lhs <= rhs && lhs != 1 && rhs != 1
    }
    def apply=x.apply * y.apply
  }
  
  case class Sub(x :Expression, y :Expression) extends Expression {
    def valid = x.apply > y.apply
    def apply=x.apply - y.apply
  }

  case class Div(x :Expression, y :Expression) extends Expression {
    def valid = {
      val lhs = x.apply
      val rhs = y.apply
      lhs % rhs == 0 && rhs != 1
    }
    def apply = x.apply / y.apply
  }
  
  def split(l:List[Int]) : List[(List[Int],List[Int])] = l match {
    case Nil => List((Nil, Nil))
    case x::xs => List((Nil,x::xs)) ::: (split(xs) map (tuple => ((x::tuple._1,tuple._2))))
  }
  
  def nonEmpty(t:Tuple2[List[Int], List[Int]]) = t._1.nonEmpty && t._2.nonEmpty
  
  def nesplit(a:List[Int]): List[(List[Int],List[Int])] = split(a) filter nonEmpty
  
  type Result = (Expression, Int)
  
  def combine(lx:Result, ry:Result):List[Result] = (lx,ry) match {
    case ((l,_),(r,_)) => List(Add(l,r),Sub(l,r),Mult(l,r),Div(l,r)) filter (_.valid) map (e => (e, e.apply))
  }
  
  def results(ns:List[Int]): List[Result] = ns match {
    case Nil => Nil
    case x::Nil if x > 0 => List((Literal(x), x)) 
    case x::xs => for { (ls,rs) <- nesplit(x::xs)
    					lx <- results(ls)
    					ry <- results(rs)
    					res <- combine(lx,ry)   					
    } yield res
  }
  
  def interleave(x:Int, xs:List[Int]):List[List[Int]] = (x,xs) match {
    case (x,Nil) => List(List(x))
    case (x, y :: ys) => List(x :: y :: ys) ::: ((interleave(x,ys)) map ( y::_))
  }
  
  def perms(l:List[Int]):List[List[Int]] = l match {
    case Nil => List(List())
    case (x::xs) => for(p0 <- perms(xs); p1 <- interleave(x, p0)) yield p1
  }
  
  def subs(l:List[Int]):List[List[Int]] = l match {
    case Nil => List(List())
    case x::xs => subs(xs) ::: (subs(xs) map (x::_))
  }
  
  def subbags(xs:List[Int]):List[List[Int]] = 
	for {ys <- subs(xs) 
	  	 zs <- perms(ys)} yield zs

  def solutions(xs:List[Int], target:Int):List[Expression] =
    for{ ns <-subbags(xs)
        (e,m) <- results(ns) if m==target
    } yield e 
    
  def format : Expression => String
    =
    _ match {
        case Literal( x ) => x.toString
        case Add(leftExpr, rightExpr) => "( " + format(leftExpr) + " + " + format(rightExpr) + " )"
        case Mult(leftExpr, rightExpr) => "( " + format(leftExpr) + " * " + format(rightExpr) + " )"
        case Sub(leftExpr, rightExpr) => "( " + format(leftExpr) + " - " + format(rightExpr) + " )"
        case Div(leftExpr, rightExpr) => "( " + format(leftExpr) + " / " + format(rightExpr) + " )"
    }
  
  def print(xs:List[Int], target:Int) = solutions(xs,target) map format  
}
