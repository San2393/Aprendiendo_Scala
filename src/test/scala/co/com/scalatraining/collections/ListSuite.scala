package co.com.scalatraining.collections

import org.scalatest.FunSuite

class ListSuite extends FunSuite {

  test("Una List se debe poder construir") {

    val lista = List(1, 2, 3, 4)
    val lista2 = 1 :: 2 :: 3 :: 4 :: 1 :: Nil
    assert(lista != lista2)
  }

  test("Una List se debe poder transformar") {

    def f(s: String): String = s + "prueba"

    val lista = List("1", "2", "3")
    val lista2 = lista.map(dato => dato + "prueba")
    val lista3 = lista.map(dato => f(dato))

    assert(lista2.head == "1prueba")
    assert(lista != lista2)
    assert(lista2 == lista3)

  }

  test("Map sobre tupla") {

    def reverse(a: Int, b: String): (String, Int) = {
      (b, a)
    }

    val lista = List[(Int, String)]((1, "Hola"), (2, "Dos"))
    val mp = lista.map(x => reverse(x._1, x._2))

    assert(mp == List(("Hola", 1), ("Dos", 2)))


  }

  test("Una lista se debe poder acumular") {
    val lista = List(1, 2, 3, 4)
    assertResult(10) {
      lista.fold(0) { (acumulado, item) =>
        acumulado + item
      }
    }
  }

  test("fold sobre una List de objetos") {
    case class MyCaseClass(i: Int, var s: String)
    val lista: List[MyCaseClass] = List(MyCaseClass(1, "1"), MyCaseClass(2, "2"))

    assertResult("12") {
      lista.map(x => x.s).fold("") { (acc, item) => acc + item }
    }

  }

  test("test dificil") {
    val lista = List(1, 2, 3, 4, 6, 7, 8, 9, 10)
    assert(true)
  }


  test("test fold") {
    val lista = List("a", "b", "c", "d")
    val l = lista.tail.foldRight("Inicio") {
      var n = 0
      (acc, item) =>
        n = n + 1
        acc + n + item
    }
  }

  test("Una lista se debe poder acumular en una direccion determinada (izquierda)") {
    val lista = List("Andres", "Felipe", "Juan", "Carlos")
    assertResult("1.Andres,2.Felipe,3.Juan,4.Carlos,") {
      var cont = 0
      lista.foldLeft("") { (resultado, item) =>
        cont = cont + 1
        resultado + cont + "." + item + ","
      }
    }
  }

  test("Una lista se debe poder acumular en una direccion determinada (derecha)") {
    val lista = List("Andres", "Felipe", "Juan", "Carlos")
    assertResult("1.Carlos,2.Juan,3.Felipe,4.Andres,") {
      var cont = 0
      lista.foldRight("") { (item, resultado) =>
        cont = cont + 1
        resultado + cont + "." + item + ","
      }
    }
  }


  test("Se debe poder consultar el primer elemento de una lista de forma insegura") {
    val lista = List(1, 2, 3, 4)
    assertResult(1) {
      lista.head
    }
  }


  test("Que pasa si hacemos head a una List()") {
    val lista = List()
    assertThrows[NoSuchElementException] {
      lista.head
    }
  }

  test("Se debe poder acceder al primer elemento de List() de forma segura") {
    val lista = List()
    val result = lista.headOption
    assert(result == None)
  }

  test("Se debe poder acceder al primer elemento de List(1,2,3) de forma segura") {
    val lista = List(1, 2, 3, 4)
    val result = lista.headOption
    assert(result == Some(1))
  }


  test("Se debe poder obtener todos los elementos de una lista sin el primer elemento") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(2, 3, 4)) {
      lista.tail
    }
  }

  test("Se debe poder obtener todos los elementos de una lista el segundo elemento") {
    val lista = List(1, 2, 3)
    val segundo = lista.tail
    assert(segundo == List(2, 3))
  }

  test("Que pasa si hacemos tail a un List()") {
    val lista = List()
    assertThrows[UnsupportedOperationException] {
      val res = lista.tail
    }
  }

  test("Quiero probar tuplas") {
    val tupla: (Int, Int, String, List[Int]) = (1, 2, "3", List(1, 2, 3))
    assert(tupla._2 == 2)
    assert(tupla._4.tail.head == 2)
  }

  test("Quiero probar tuplas 2 ") {
    val tupla = (1, 2, "3", List(1, 2, 3), "Hola")
    assert(tupla._5 == "Hola")
  }

  test("Una lista se debe poder dividir") {
    val lista = List(1, 2, 3, 4)
    val t = lista.splitAt(2)
    assert(t._1 == List(1, 2) && t._2 == List(3, 4))
  }

  test("Se puede hacer una lista de un solo tipo") {
    val lista = List(1, 2, "3")
    val t = lista.splitAt(2)
    assertDoesNotCompile("val l = List[String](\"1\", \"2\", 3)")
  }

  test("Una lista se debe poder reversar") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(4, 3, 2, 1)) {
      lista.reverse
    }
  }

  test("A una lista se le debe poder eliminar elementos con drop") {
    val lista = List(1, 2, 3, 4)
    val dropped = lista.drop(2)

    assertResult(List(3, 4)) {
      dropped
    }
  }


  test("A una lista se le debe poder eliminar elementos hasta donde") {
    val lista = List(1, 9, 7, 5, 3, 4)
    val dropped = lista.drop(5)
    assertResult(List(4)) {
      dropped
    }
  }

  test("A una lista se le pueden descartar elementos en una direccion determinada (right)") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(1, 2)) {
      lista.dropRight(2)
    }
  }


  test("Una lista se debe poder filtrar con una hof") {
    val lista = List(1, 2, 3, 4)
    assertResult(List(2, 4)) {
      lista.filter(x =>
        x % 2 == 0
      )
    }
  }

  test("Una lista se debe poder filtrar con una hof con ejemplos") {
    val lista = List(1, 2, 3, 4)

    def isPar(x: Int): Boolean = x % 2 == 0

    assertResult(List(2, 4)) {
      lista.filter(x => isPar(x))
    }
    // _
    assertResult(List(2, 4)) {
      lista.filter(_ % 2 == 0)
    }

    //solo funcion
    assertResult(List(2, 4)) {
      lista.filter(isPar)
    }
  }

  test("Una lista se debe poder recorrer imperativamente") {
    val lista = List(1, 2, 3, 4)
    assertResult(10) {
      var sum = 0
      lista.foreach((x) =>
        sum += x
      )
      sum
    }
  }

  test("Una lista se debe poder serializar") {
    val lista = List(1, 2, 3, 4)
    assertResult("1&2&3&4") {
      lista.mkString("&")
    }
  }

  test("Se pueden poder sumar los elementos de una lista") {
    val lista = List(1, 2, 3, 4)
    assertResult(10) {
      lista.sum
    }
  }


}
