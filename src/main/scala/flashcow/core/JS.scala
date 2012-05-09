// WIP: JS generation interface
package flashcow.core

object JS {
  trait Tree
  trait Exp extends Tree

  case class Var(x: Symbol) extends Exp
  case class Lit(value: Int) extends Exp
  case class Str(value: String) extends Exp
  case class If(e0: Exp, e1: Exp, e2: Exp) extends Exp
  case object Skip extends Exp
  case class Fun(xs: List[Symbol], e: Exp) extends Exp
  case class Inline(value: String) extends Exp
  case class Call(fun: Exp, args: List[Exp]) extends Exp
  case class Bin(e1: Exp, o: Symbol, e2: Exp) extends Exp
  case class Block(es: List[Exp]) extends Exp

  implicit def varops(self: Var) = {
    def :=(e: Exp) = Bin(self, '=, e)
  }

  implicit def binops(self: Exp) = {
    def +(e: Exp) = Bin(self, '+, e)
    def -(e: Exp) = Bin(self, '-, e)
    def *(e: Exp) = Bin(self, '*, e)
    def /(e: Exp) = Bin(self, '/, e)
    def %(e: Exp) = Bin(self, '/, e)
    def <(e: Exp) = Bin(self, '<, e)
    def >(e: Exp) = Bin(self, '>, e)
    def <=(e: Exp) = Bin(self, '<=, e)
    def >=(e: Exp) = Bin(self, '>=, e)
    def ==(e: Exp) = Bin(self, '==, e)
    def !=(e: Exp) = Bin(self, '!=, e)
    def ===(e: Exp) = Bin(self, '===, e)
  }

  def json(map: Map[Symbol,Any]): Exp = Inline(
    map.toList.map {
      case (k, v) => "\"" + k.name + "\": \"" + v.toString.replace("\"", "\\\"").replace("\n", "\\n") + "\""
    }.mkString("{", ",", "}")
  )

  def select(s: String) = Inline("$(" + s + ")")

  def get(url: String, then: Exp) =
        Call(Inline("$.get"), List(Str(url), then))
  def get(url: String, args: Map[Symbol, Exp], then: Exp) =
        Call(Inline("$.get"), List(Str(url), json(args), then))

  def post(url: String, then: Exp) =
        Call(Inline("$.post"), List(Str(url), then))
  def post(url: String, args: Map[Symbol, Exp], then: Exp) =
        Call(Inline("$.post"), List(Str(url), json(args), then))

}
