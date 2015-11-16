package net.cabworks.evalEdn

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

/**
 * Created by cab on 15/11/2015.
 */
object EdnRepl {
  def replLoop : Unit = {
    val input = StdIn.readLine("Edn> ")
    input match {
      case ":q" => ()
      case "(exit)" => ()
      case in : String if in.length() > 0 => {
        val result = Try(EdnEvaluator.evalString(in)) match {
          case Success(res) => res
          case Failure(ex) => println(ex.getMessage)
        }
        println(result)
        replLoop
      }
      case _ => ()
    }

  }

  def main(args: Array[String]) = replLoop
}
