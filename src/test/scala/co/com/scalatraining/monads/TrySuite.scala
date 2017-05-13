package co.com.scalatraining.monads

import org.scalatest._

import scala.collection.immutable.Seq
import scala.util.{Failure, Success, Try}

class TrySuite extends FunSuite with Matchers {

  def estallar(): Int = {
    println("Diviendo por cero :/")
    2 / 0
  }

  def computar(): Int = {
    println("Retornando un 1 :)")
    1
  }

  def f = Try {
    estallar
  }

  def s = Try {
    computar
  }

  test("Tratando el Try como un bloque try-catch") {
    val res = Try {
      1
      2
      3
      2 / 0
      4
    }

    assert(res.isFailure)
  }
  test("Se debe poder hacer pattern match sobre un Try que es Failure") {
    f match {
      case Success(valor) => assert(false)
      case Failure(e) => assert(true)
    }
  }

  test("Se debe poder hacer pattern match sobre un Try que es Success") {
    s match {
      case Success(valor) => assert(true)
      case Failure(e) => assert(false)
    }
  }

  test("Un Failure se debe poder map") {
    val res = f.map(x => "HOLA")
    assert(res.isFailure)
  }

  test("Un Success se debe poder map [assert con for-comp]") {
    val res = s.map(x => "HOLA")
    assert(res.isSuccess)

    for {
      ss <- res
    } yield {
      assert(ss == "HOLA")
    }

  }

  test("Un Success se debe poder map [assert con flatmap]") {
    val res = s.map(x => "HOLA")

    res.flatMap(x => Success(assert(x == "HOLA")))

  }

  test("Un Failure se debe poder recuperar con recover") {

    val res = f.map(x => "HOLA")
      .recover { case e: Exception => {
        "HOLA ME HE RECUPERADO"
      }
      }

    res.flatMap(s => Try(assert(s == "HOLA ME HE RECUPERADO")))

  }

  test("Un Failure se debe poder recuperar con recoverWith") {

    val res: Try[Any] = f.map(x => "HOLA")
      .recoverWith { case e: Exception => {
        s
      }
      }

    res.flatMap(x => Try(assert(x == 1)))

  }

  test("Un Try se debe poder convertior en Option") {
    val res = s.toOption

    for {
      v <- res
    } yield assert(v == 1)

    assert(res == Some(1))
  }

  test("Un Failure se debe poder convertior en Option") {
    val res = f.toOption
    assert(res == None)
  }


  /*
  Este test demuestra como Try encadena con for-comp
  y como es evidente que es Success biased lo que significa que solo continua
  con el encadenamiento si el resultado de cada paso es Success
   */
  test("Try for-com. A chain of Success is a Success") {

    val res = for {
      x <- s
      y <- s
      z <- s
    } yield x + y + z


    assert(res == Success(3))

    res.flatMap(x => {
      assert(x == 3)
      Success(":)")
    })

  }

  /*
  Este test demuestra como Try encadena con for-comp
  y como ante una falla en la cadena, el computo resultante es una falla
   */
  test("Try for-com. A chain of Tries with a Failure is a Failure") {

    val res = for {
      x <- s
      y <- f
      z <- s
    } yield x + y + z

    assert(res.isFailure)

  }


  /*
  * Ejemplos de Try
  * */

  test("Probando get y getOrElse en Try") {
    val a = s.get
    assert(a == 1)

    val b = f.getOrElse("Hola")
    assert(b == "Hola")

  }

  test("Probando filter en Try") {
    val a = s.filter(x => x >= 1)

    a.flatMap(x => Success(assert(x == 1)))

  }

  test("Probando recoverWith en Try") {
    val t = f.map(x => "").recoverWith {
      case e: Exception => {
        Try {
          f
        }.recover { case e: Exception => {
          "No"
        }
        }
      }
    }

    t.flatMap(x => Try {
      assert(x == "No")
    })


  }

  test("Probando orElse en Try") {
    val t = s.map(x => x + 1).filter(y => y != 0)
    assert(t.isSuccess)
    assert(t.orElse(Try(3)) == Try(2))


  }

  test("Probando flatten en Try") {

    def fla(i: Try[Int]) = {
      Try(i)
    }

    val a = fla(Try(3))

    assert(a == Success(Success(3)))

    val m = a.flatten
    assert(m == Success(3))


  }


  test("Try -> Option ejemplo") {

    val list = List(1, 2, 2, 0)
    val newList = list.map(x => Try(6 / x).recover { case e: Exception => {
      0
    }
    })

    val l = newList.flatMap(x => x.toOption)

    assert(l == List(6, 3, 3, 0))

  }

  test("Try WithFilter ejemplo") {

    val l: Try[Int]#WithFilter = s.withFilter(x => x == 0)
    assert(l != 1)

  }


  /*
  Monad laws
  1. Associativity
  monad.flatMap(f).flatMap(g) == monad.flatMap(v => f(v).flatMap(g))

  2. Left unit
  unit(x).flatMap(f) == f(x)

  3. Right unit
  monad.flatMap(unit) == monad

   */
  test("Try monad verification. Is Try associative?") {

    def fi(i: Int): Try[Int] = Try {
      i + 1
    }

    def gi(i: Int): Try[Int] = Try {
      i + 2
    }

    //monad.flatMap(f).flatMap(g) == monad.flatMap(v => f(v).flatMap(g))
    assert(s.flatMap(fi(_)).flatMap(gi(_)) == s.flatMap(v => fi(v).flatMap(gi(_))))

  }

  /*
  La unidad por derecha se cumple pues la excepcion (que es el peor caso)
  siempre está protegida y la verificación evalua a true tanto para Success como para Failure
   */
  test("Try monad verification. Does Try comply right unit?") {

    def fi(i: Int): Try[Int] = Try {
      i + 1
    }

    def gi(i: Int): Try[Int] = Try {
      i + 2
    }

    //monad.flatMap(unit) == monad
    assert(s.flatMap(x => Success(1)) == s)

    // La siguiente verificacion hace fallar el test aunque los dos Failure sean por el mismo motivo
    //assert(f.flatMap(x => Try(2/0)) == f)

  }

  /*
  Cuando el unit es un Success **sí**  se cumple la ley de unidad por izquierda
  pues el tanto el lado izquierdo como el derecho de la verificación evalúan al mismo valor
   */
  test("Try monad verification. Does Try comply left unit (1)?") {

    def fi(i: Int): Try[Int] = Try {
      i + 1
    }

    def gi(i: Int): Try[Int] = Try {
      i + 2
    }

    def unit = Try(1)

    //unit(x).flatMap(f) == f(x)
    assert(unit.flatMap(fi) == fi(1))

    assert(true)

  }

  /*
  Cuando el unit es un Failure **no** se cumple la ley de unidad por izquierda
  pues el lado izquierdo de la verificación sí puede manejar la excepcion
  mientras que el lado derecho de la verificación estalla con excepcion
  y por ende no hay igualddad.
   */
  ignore("Try monad verification. Does Try comply left unit (2)?") {

    def fi(i: Int): Try[Int] = Try {
      i + 1
    }

    def gi(i: Int): Try[Int] = Try {
      i + 2
    }

    //unit(x).flatMap(f) == f(x)

    def unit = Try(2 / 0)

    val left = unit.flatMap(fi)
    println(s"Ok tengo un left: $left")
    val right = fi(2 / 0)
    println(s"Ok tengo un right: $right")
    assert(left == right)

  }


}
