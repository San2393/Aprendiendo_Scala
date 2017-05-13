package co.com.scalatraining.collections

import org.scalatest.FunSuite

/**
  * Created by santiago herrera on 8/02/17.
  */
class RangeSuite extends FunSuite {

  test("Uso de range con list") {
    val rg = 1 to 3
    assertResult(List(1, 2, 3)) {
      rg.toList
    }
  }

  test("Creando un array con range") {
    val arr = Array.range(1, 4)
    assertResult(Array(1, 2, 3)) {
      arr
    }
  }

  test("Inicializar lista con range") {
    val lt = List.range(1, 4)
    assertResult(List(1, 2, 3)) {
      lt
    }
  }

  test("Usando rango con map") {
    val x = (1 to 5).map {
      num => num * 2
    }

    assertResult(Seq(2, 4, 6, 8, 10)) {
      x
    }
  }
}
