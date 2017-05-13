package co.com.scalatraining.syntax

import org.scalatest.FunSuite

import scala.util.Either.{RightProjection, LeftProjection}


class EitherSuite2_11 extends FunSuite {

  test("Construyendo en Either") {
    val r: Right[Nothing, Int] = Right(1)
    val l: Left[String, Nothing] = Left("Hola sy un left")

    assert(r.isRight)
    assert(l.isLeft)
  }


  test("Probando Either con un par") {

    def foo(i: Int): Either[String, Int] = {

      if (i % 2 == 0)
        Right(i)
      else
        Left("Soy impar :(")
    }

    assert(foo(2) == Right(2))


  }

  test("Probando Either con swap invirte valores") {

    val r = Right("Soy derecha")
    val i = Left("Soy Izquierda")

    assert(r.swap == Left("Soy derecha"))
    assert(i.swap == Right("Soy Izquierda"))


  }


  test("Probando Either con fold") {

    val r = Right("Soy derecha")
    val l = Left("Soy Izquierda")

    val a = r.fold(y => "no" + y, x => "Si " + x)
    val b = l.fold(y => "Si " + y, x => "no" + x)


    assert(a == "Si Soy derecha")
    assert(b == "Si Soy Izquierda")

  }

  test("Probando Either con joinRight y joinLeft analogia con flatten") {

    val r = Left(Right("Soy derecha"))
    val l = Right(Left("Soy Izquierda"))

    val a = l.joinRight
    val b = r.joinLeft


    assert(a == Left("Soy Izquierda"))
    assert(b == Right("Soy derecha"))

  }


  test("Probando Either con merge extrea el valor") {

    val r = Right(1)
    val l = Left("Soy Izquierda")

    val a = r.merge
    val b = l.merge


    assert(a == 1)
    assert(b == "Soy Izquierda")

  }

  test("Probando Either con las envolturas left(LeftProjection) y right(RightProjection) ") {

    val r = Right("Soy derecha")
    val l = Left("Soy Izquierda")

    val a = r.right
    val b = l.left


    assert(a == RightProjection(Right("Soy derecha")))
    assert(b == LeftProjection(Left("Soy Izquierda")))

  }

  test("Probando Either  map en LeftProjection con map") {

    val l = Left("Soy Izquierda")

    val b = l.left


    assert(b.map(x => x + " 1") == Left("Soy Izquierda 1"))

  }
  test("Probando Either  map en rightProjection con flapMap") {

    val r = Right("Soy Derecho")

    val b = r.right


    assert(b.flatMap(x => Right(x + " 1")) == Right("Soy Derecho 1"))

  }


}
