package net.cabworks.evalEdn

import net.cabworks.edn4scala.EdnParser.{EdnList, EdnSymbol, EdnKeyword}
import org.scalatest.{FreeSpec, Matchers}

/**
 * Created by cab on 14/11/2015.
 */
class EdnEvaluatorTest extends FreeSpec with Matchers {
  def eval(input : String): Any = EdnEvaluator.evalString(input)
  val sym = TestHelpers.sym(_)

  "self evaluating" in {
    eval("1") should be(1)
    eval("\"TEST STRING\"") should be ("TEST STRING")
    eval(":d")  should be(EdnKeyword("d"))
    eval(":namespace1/test") should be(EdnKeyword("namespace1", "test"))
  }

  "quoted lists" in {
    eval("'(1 2 2)")  should be(EdnList(List(1, 2, 2)))
    eval("'(32 2 2)") should be(EdnList(List(32, 2, 2)))
    eval("'(/ 2 2)")  should be(EdnList((List(EdnSymbol("/"), 2, 2))))
    eval("'(* 6 7)")  should be(EdnList(List(EdnSymbol("*"), 6, 7)))
  }

  "calculator" in {
    eval("(+ 2 2)") should equal(4)
    eval("(- 2 2)") should equal(0)
    eval("(/ 2 2)") should equal(1)
    eval("(* 6 7)") should equal(42)

  }


  "multi expressions" in {
    eval("(- 2 2 ) (+ 2 2)") should equal(0)
  }

  "nested expressions" in {
    eval(" (+ (* 2 2) 2)") should equal(6)
    eval(" (+ (* (- 24 10) 2) (/ 15 3))") should equal(33)

  }

  "ifs" in {
    eval("(if true \"True indeed\"  \"Deeply false\")") should equal( "True indeed" )
    eval("(if false \"True indeed\"  \"Deeply false\")") should equal("Deeply false")

    eval("(if (= 2 2) \"True indeed\"  \"Deeply false\")") should equal("True indeed")
    eval("(if (not= 2 2) \"True indeed\"  \"Deeply false\")") should equal("Deeply false")


    eval("(if (> 3 2) \"True indeed\"  \"Deeply false\")") should equal("True indeed")
    eval("(if (< 2 2) \"True indeed\"  \"Deeply false\")") should equal("Deeply false")

    eval("(if (>= 2 2) \"True indeed\"  \"Deeply false\")") should equal("True indeed")

    eval("(if (>= 4 2) \"True indeed\"  \"Deeply false\")") should equal("True indeed")
    eval("(if (<= 2 2) \"True indeed\"  \"Deeply false\")") should equal("True indeed")
    eval("(if (<= 0 2) \"True indeed\"  \"Deeply false\")") should equal("True indeed")

    eval("(if (<= 3 2) \"True indeed\"  \"Deeply false\")") should equal("Deeply false")


    eval("(if (and true (> 2 1)) \"True indeed\"  \"Deeply false\")") should equal("True indeed")
    eval("(if (and true (> 2 2)) \"True indeed\"  \"Deeply false\")") should equal("Deeply false")


    eval("(if (and true (> 2 1) (< 0 23)) \"True indeed\"  \"Deeply false\")") should equal("True indeed")
    eval("(if (and true (> 2 1) (< 0 23) (= 4 (+ 2 2))) \"True indeed\"  \"Deeply false\")") should equal("True indeed")
    eval("(if (and true (> 2 1) (< 0 23) (= 4 (+ 2 3))) \"True indeed\"  \"Deeply false\")") should equal("Deeply false")
  }

  "def" in {

    eval("(def b 72)")
    eval("b") should be(72)

    eval("(def b (+ 2 30 (* 4 10)))") should be(72)

    eval("(def b (+ 2 30 (* 4 10)))")
    eval("b") should be(72)


    eval("(def b 3)")
    eval("(+ b 7)") should be(10)

  }

  "lambda" in {
    eval("(fn [a b] (+ a b))") should be(Vector(sym("a"), sym("b")), List(EdnList(List(sym("+"), sym("a"), sym("b")))))

    eval("(fn [a b] (+ a b))") should be(Vector(sym("a"), sym("b")), List(EdnList(List(sym("+"), sym("a"), sym("b")))))
  }

  "apply" in {
    eval("(def f (fn [a b] (+ a b)))")
    eval("(f 1 1)") should be(2)


    eval("((fn [a b] (+ a b)) 11 11)") should be(22)

    eval("(def f (fn [a b] (* (+ a b) (- 5 a))))")
    eval("(f 2 1)") should be(9)

  }

}

object TestHelpers {
  def sym(s : String) : EdnSymbol= {
    EdnSymbol(s)
  }
}

