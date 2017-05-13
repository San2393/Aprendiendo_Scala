package co.com.scalatraining.collections

import org.scalatest.FunSuite

import scala.collection.immutable.Stream.cons

/**
  * Created by janus on 9/02/17.
  */
class ForExpressionsSuite extends FunSuite {

  test("Smoke test") {
    assert(true)
  }

  test("for-comp imperativo 1") {
    val l = 1 to 10

    var cont = 0
    for {
      a <- l
    } cont += 1

    assert(cont == 10)
  }

  test("for-comp imperativo 2") {
    case class CC()

    assertDoesNotCompile {
      "for{ " +
        " a <- CC() " +
        "} println(a)"
    }

  }


  test("Test for-comp en list") {
    val b = List(1, 2, 3, 4)
    var cont = 0
    for {
      a <- b
    } cont += 1

    assert(cont == 4)
  }

  test("Usando range en for") {
    var a = "no"
    for (i <- Range(0, 2, 1)) {
      a = s"range $i"
    }

    assert(a == "range 1")
  }

}
