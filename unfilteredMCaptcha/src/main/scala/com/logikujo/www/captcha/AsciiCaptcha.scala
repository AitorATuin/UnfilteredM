package com.logikujo.www.captcha

import scala.xml.NodeSeq
import scala.sys.process._
import scala.util.Random
/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.captcha 22/04/14 :: 22:05 :: eof
 *
 */
trait AsciiCaptcha {
  sealed trait MathEquation {
    protected lazy val random = new Random(System.currentTimeMillis)
    private def newNumber = random.nextInt(9) + 1
    private def newOp = random.nextInt(3)
    private val ops: Map[Int,(Char, (Int,Int) => Int)] = Map(
      0 -> ('+' -> ((a,b) => a + b)),
      1 -> ('*' -> ((a,b) => a * b)),
      2 -> ('-' -> ((a,b) => a - b)))
    val solution: Int = newNumber
    protected val terms:List[Int] = List(solution)
    override def toString = terms.zipWithIndex.map {
      case (v, i) if i%2 == 0 => v.toString
      case (v,i) => ops(v)._1.toString
    } mkString("")
    def addTerm: MathEquation = {
      val n = newNumber
      val op = newOp
      val a = this.solution
      val t = this.terms
      val r = this.random
      new MathEquation {
        override protected lazy val random = r
        override val solution = ops(op)._2(a, newNumber)
        override protected val terms = n :: op :: t
      }
    }
  }
  object MathEquation {
    def apply(): MathEquation = new MathEquation {}
    def apply(n: Int): MathEquation =
      if (n > 1)
        (1 to n - 1).toList.foldLeft(MathEquation())((eq, i) => eq.addTerm)
      else
        MathEquation()
  }
  val eq = MathEquation(4)
  def question: NodeSeq =
    <pre>
{s"figlet ${eq.toString}".!!}
    </pre>
  def solve(a: Int) = eq.solution == a
}
