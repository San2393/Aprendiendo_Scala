package co.com.scalatraining.collections

import org.scalatest.FunSuite

/**
  * Created by santiago herrera on 8/02/17.
  */
class VectorSuite extends FunSuite {

  test("Creando vector y usando map en un vector") {
    val vec = Vector(1, 2, 3)
    val vec2 = vec.map(dato => dato + "prueba")
    assert(vec != vec2)
  }

  test("Uso head en un vector") {
    val vec = Vector(1, 2, 3, 4)
    assertResult(1) {
      vec.head
    }
  }


  test("Usando tail en un vector") {
    val vec = Vector(1, 2, 3, 4)
    assertResult(Vector(2, 3, 4)) {
      vec.tail
    }
  }

  test("Uso de split en un vector") {
    val vec = Vector(1, 2, 3, 4)
    val (vec2, vec3) = vec.splitAt(2)
    assert(vec2 == Vector(1, 2) && vec3 == Vector(3, 4))
  }

  test("se puede usar reverse") {
    val vec = Vector(1, 2, 3, 4)
    assertResult(Vector(4, 3, 2, 1)) {
      vec.reverse
    }
  }

  test("usando apply en vector") {
    val vec = Vector(9, 1, 2, 3, 4)
    assertResult(9) {
      vec.apply(0)
    }
  }

  test("drop en vector") {
    val vec = Vector(1, 2, 3, 4)
    assertResult(Vector(3, 4)) {
      vec.drop(2)
    }
  }

  test("dropRight vector") {
    val vec = Vector(1, 2, 3, 4)
    assertResult(Vector(1, 2)) {
      vec.dropRight(2)
    }
  }


  test("filter en vector") {
    val vec = Vector(1, 2, 3, 4)
    assertResult(Vector(2, 4)) {
      vec.filter(x =>
        x % 2 == 0
      )
    }
  }

  test("foreach en vector") {
    val vec = Vector(1, 2, 3, 4)
    assertResult(10) {
      var sum = 0
      vec.foreach((x) =>
        sum += x
      )
      sum
    }
  }

  test("mkString en vector") {
    val vec = Vector(1, 2, 3, 4)
    assertResult("1&2&3&4") {
      vec.mkString("&")
    }
  }

  test("sum en vector") {
    val vec = Vector(9, 2, 3, 4)
    assertResult(18) {
      vec.sum
    }
  }


  test("take en vector") {
    val vec = Vector(9, 2, 3, 4)
    assertResult(Vector(9)) {
      vec.take(1)
    }
  }

  test("update en vector") {
    val vec = Vector(9, 2, 3, 4)
    assertResult(Vector(8, 2, 3, 4)) {
      vec.updated(0,8)
    }
  }
}
