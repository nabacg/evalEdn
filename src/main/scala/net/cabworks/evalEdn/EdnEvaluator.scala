package net.cabworks.evalEdn

import clojure.lang.{Keyword, Symbol}
import net.cabworks.edn4scala.EdnParser
import net.cabworks.edn4scala.EdnParser._

/**
 * Created by cab on 14/11/2015.
 */
object EdnEvaluator {

  def evalString(input : String) : Any = eval(initEnv, EdnParser.readEdnString(input))

  def eval(env : Map[EdnSymbol,Any], e : Any) : Any = e match {
    case b: Boolean => b
    case n: Number => n
    case s: String => s
    case k: EdnKeyword => k
    case s: EdnSymbol => lookupSymbol(env, s)
    case EdnList( EdnSymbol(_, "if", _) :: pred :: thenExpr :: elseExpr :: Nil, _) => eval(env, pred) match { case true => eval(env, thenExpr); case false => eval(env, elseExpr)}
    case EdnList( EdnSymbol(_, "def", _) :: (nameSym: EdnSymbol) :: value :: Nil, _) =>  updateEnv(nameSym, eval(env, value))
    case EdnList(EdnSymbol(_, "fn", _) :: EdnVector(params: Vector[EdnSymbol], _) :: body, _ )  => defineFunction(params, body)
    case EdnList(EdnList(EdnSymbol(_, "fn", _) :: EdnVector(params: Vector[EdnSymbol], _) :: body, _ ) :: args, _) => eval(extendEnv(env, params, args), body)
    case EdnList(EdnSymbol(_, "quote", _) :: tail, _)  => tail.head
    case EdnList((f : EdnSymbol) :: args, _)=> apply(f, args, env)
    case exprs: List[EdnExpr] => exprs.map(e => eval(env, e)).last

  }

  def apply(f : EdnSymbol, args : Seq[Any], env : Map[EdnSymbol, Any]) : Any = f match {
    case f if primitiveProcedures.contains(f) => apply(eval(env, f).asInstanceOf[(Any) => Any], args.map(eval(env, _)))
    case f => env.get(f) match {
        case Some((params : Seq[EdnSymbol], body)) => eval(extendEnv(env, params, args), body)
        case None => sys.error(s"Unknown symbol $f make sure value is declared before using it!")
        case _ => sys.error(s"Symbol $f is not a function or primitive operator, are you trying to call a non function symbol?")
      }

  }

  def defineFunction(params: Vector[EdnSymbol], body: Any): Any = (params, body)

  def createMacro(key: EdnSymbol, value: Any): Any = macros = macros + (key -> value)


  def extendEnv(env : Map[EdnSymbol, Any], ks : Seq[EdnSymbol], vals : Seq[Any]) = env ++ ks.zip(vals)

  def apply[T, R](f : (T) => R, args : T) = f(args)

  def initEnv: Map[EdnSymbol, Any] = {
    environment = primitiveProcedures ++ environment
    environment
  }
  def updateEnv(k : EdnSymbol, v : Any) = {
    environment = environment + (k -> v)
    v
  }

  def numericBinaryOperator[T](op : (T , T) => T)(args : Seq[T]) : T = args.reduce(op) //args.tail.foldLeft(args.head)((a, i) => op(a, i))
  def boolBinaryOperator[T](op : (T, T) => Boolean)(args : Seq[T]) : Boolean = args match {
      case l :: r :: Nil => op(l, r)
      case _ => sys.error(s"Can't apply arguments: $args to operator: $op")
    }

  def lookupSymbol(env : Map[EdnSymbol, Any], s : EdnSymbol) : Any = env.get(s) match {
    case Some(a) => a
    case None => sys.error("Unknown symbol " + s.toString + " make sure value is declared before using it!")
  }

  var environment : Map[EdnSymbol, Any] = Map(EdnSymbol("a") -> 42)
  var macros : Map[EdnSymbol, Any] = Map()

  val primitiveProcedures: Map[EdnSymbol, Any] = Map(
    EdnSymbol("+") -> numericBinaryOperator((a : Long, b: Long) => a+b)_,
    EdnSymbol("-") -> numericBinaryOperator((a : Long, b: Long) => a - b)_,
    EdnSymbol("*") -> numericBinaryOperator((a : Long, b: Long) => a * b)_,
    EdnSymbol("/") -> numericBinaryOperator((a : Long, b: Long) => a / b)_,
    EdnSymbol("=") -> boolBinaryOperator((a : Long, b: Long ) => a == b)_,
    EdnSymbol("not=") -> boolBinaryOperator((a : Long , b : Long) => a != b)_,
    EdnSymbol("not=") -> boolBinaryOperator((a : Long , b : Long) => a != b)_,
    EdnSymbol("<") -> boolBinaryOperator((a : Long , b : Long) => a < b)_,
    EdnSymbol("<=") -> boolBinaryOperator((a : Long , b : Long) => a <= b)_,
    EdnSymbol(">") -> boolBinaryOperator((a : Long , b : Long) => a > b)_,
    EdnSymbol(">=") -> boolBinaryOperator((a : Long , b : Long) => a >= b)_,
    EdnSymbol("and") -> numericBinaryOperator((a : Boolean , b : Boolean) => a && b)_,
    EdnSymbol("or") -> numericBinaryOperator((a : Boolean , b : Boolean) => a || b)_
  )  // this is quite ridiculous though.. // todo find a way to make it work on Numerics without hardcoded ints!

}
