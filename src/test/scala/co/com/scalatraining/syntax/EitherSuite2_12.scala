package co.com.scalatraining.syntax

import org.scalatest.FunSuite

import scala.util.Success


class EitherSuite2_12 extends FunSuite {

  /*
  * En la la version 2.12 ya se puede hacer directamente sobre el Either muchos mas funciones, que en la
  * version 2.11 tenia que ultilizar el rightProjection o leftProjection para acceder a estas.
  * */


  test("Probando map en Either") {

    val r = Right(1215)
    val l = Left(45)
    val res = r.map(x => "R" * 3)
    val res2 = l.map(x => "R" * 3)

    assert(res == Right("RRR"))
    assert(res2 == Left(45))

  }


  test("Probando flapMap en Either") {

    val r = Right(1)
    val res = r.flatMap(x => Right(x * 5))

    assert(res == Right(5))

  }


  test("Probando Option en Either") {


    val r = Right(1)
    // Lo toma como valor
    val l = Left(15646)
    // Lo toma como None
    val res1 = r.toOption
    val res2 = l.toOption

    assert(res1 == Option(1))
    assert(res2 == None)

  }

  test("Probando getOrElse en Either") {

    def foor(i: Int) = {
      Right(i + 6 + 4)
    }

    def fool(i: Int) = {
      Left(i + 3 + 9)
    }


    assert(fool(9).getOrElse(0) == 0)
    assert(foor(1).getOrElse(0) == 11)

  }

  test("Probando toTry en Either") {

    val a = Right(9)


    assert(a.toTry == Success(9))

  }

  test("Probando filterOrElse en Either") {

    val a = Right(9)
    val b = Left(5)

    val res1 = a.filterOrElse(x => x % 2 == 0, 1)
    val res2 = b.filterOrElse(x => x == 0, 1)


    assert(res1 == Left(1))
    assert(res2 == Left(5))

  }

}
