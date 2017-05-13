package co.com.scalatraining.collections

import org.scalatest.FunSuite

class MapSuite extends FunSuite {

  test("Creacion vacia") {
    val mapa1 = Map()
    val mapa2 = Map.empty
    assert(mapa1.isEmpty)
    assert(mapa2.isEmpty)
  }

  test("Un Map se debe poder operar en un for-comp") {
    val mapa = Map(1 -> "uno", 2 -> "dos")

    val res = for {
      i <- mapa
      if i._1 == 1
    } yield (i)

    assert(res.keys.size === 1)
    assert(res.keys.head === 1)
    assert(res.get(mapa.keys.head).get === "uno")
  }

  test("mapValue en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 4, "3" -> 9)) {
      map.mapValues(valor => valor * valor)
    }
  }

  test("head en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult("1" -> 1) {
      map.head
    }
  }


  test("tail en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("2" -> 2, "3" -> 3)) {
      map.tail
    }
  }

  test("split en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    val (map2, map3) = map.splitAt(2)
    assert(map2 == Map("1" -> 1, "2" -> 2) && map3 == Map("3" -> 3))
  }

  test("crear nuevo Map con un item mas") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4)) {
      map + ("4" -> 4)
    }
  }


  test("drop en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("2" -> 2, "3" -> 3)) {
      map.drop(1)
    }
  }

  test("dropRight en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(Map("1" -> 1, "2" -> 2)) {
      map.dropRight(1)
    }
  }


  test("filter en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3, "4" -> 4)
    assertResult(Map("2" -> 2, "4" -> 4)) {
      map.filter(dato =>
        dato._2 % 2 == 0
      )
    }
  }

  test("foreach en un Map") {
    val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
    assertResult(6) {
      var sum = 0
      map.foreach((x) =>
        sum += x._2
      )
      sum
    }
  }


  test("test ejemplo tupla") {
    val map: Map[String, (Int, Int, Int)] = Map("Nota1" -> (1, 2, 3), "Nota2" -> (2, 4, 5), "Nota3" -> (5, 6, 7))

    assert(map.head == ("Nota1", (1, 2, 3)))

  }


  test("Ejemplo map con trait") {
    trait a {
      val map: Map[String, Int]
    }

    object Obj extends a {
      val map = Map("Nota1" -> 5, "Nota2" -> 4, "Nota3" -> 9)
    }

    val primera = Obj.map


    assertResult(Map("Nota1" -> 5, "Nota2" -> 4)) {
      primera.filter(dato => dato._2 <= 5)
    }

  }


  test("prueba uso Map y List") {
    val mp = Map("k1" -> List(4, 2), "k2" -> List(7, 5), "k3" -> List(9, 3))
    val mp2 = mp.mapValues(valor => valor.filter(dato => dato > 5))


    assertResult(9) {
      mp2("k3").head
    }

  }

}
