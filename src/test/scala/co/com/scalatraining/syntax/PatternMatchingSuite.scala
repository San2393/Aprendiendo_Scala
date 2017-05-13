package co.com.scalatraining.syntax

import org.scalatest.FunSuite

/**
  * Created by santiago herrera  on 9/02/17.
  */
class PatternMatchingSuite extends FunSuite {


  test("Primer prueba uso de match") {
    val n = 2
    assertResult("Dos") {
      n match {
        case 1 => "Uno"
        case 2 => "Dos"
        case x => s"no lo se $x"

      }

    }


  }

  test("Capturando valor match") {
    val m = 4
    val n: String = m match {
      case 1 => "Uno"
      case 2 => "Dos"
      case x => s"no lo se $x"
    }
    assert(n == "no lo se 4")
  }

  test("Usando list con match") {
    val ls = List(1, 2, 3)

    assertResult("Primero 1") {
      ls match {
        case x :: xs => "Primero " + x
        case _ => "Otro"
      }
    }


  }


  test("Usando list con match detectando numero") {
    val ls = List(1)

    assertResult("Uno") {
      ls match {
        case x :: Nil => "Uno"
        case x :: xs :: Nil => "Dos"
        case _ => "Mas"
      }
    }

  }

  test("Usando match con clases") {
    case class Persona(id: String, nombre: String, edad: Int)

    val a = Persona("Key1", "Ele", 8)

    assertResult("Ele") {
      a match {
        case Persona(_, "Sam", 3) => "Sam"
        case Persona(_, _, 8) => "Ele"
        case _ => "No es"
      }
    }

  }

  test("Usando match con def") {


    def Num(x: String): Int = x match {
      case "Uno" => 1
      case "Dos" => 2
      case _ => 0
    }

    assertResult(0) {
      Num("Cuatro")

    }

  }


  test("Otro test sobre el uso de def en match") {
    def Adivina(choice: Int): String = choice match {
      case 1 | 2 | 3 => "Si"
      case 0 => "No"
      case _ => "Error"
    }

    assert(Adivina(2) == "Si")

  }


  test("Usando match con tuplas") {

    val t = (8, 2, 5)

    assertResult("Primero 8") {
      t match {
        case (a, _, _) => s"Primero $a"
        case _ => "Otros"
      }
    }


  }


  test("Uso de match con if") {
    val n = 5

    val num = n match {
      case m if m > 4 => "Es mayor a 4"
      case m if m < 1 => "Es menor a 1"
      case _ => "Medio"
    }

    assert(num == "Es mayor a 4")


  }

  test("Uso de match con ejemplo") {
    case class Profesor(nombre: String)
    class Curso(nombre: String, profesor: Profesor)

    trait t1 {
      val a = "NO"
    }

    trait t2 {
      val b = "SI"
    }

    val cs1 = new Curso("Scala", Profesor("JP")) with t1
    val cs2 = new Curso("JS", Profesor("Zulu")) with t2


    def foo(c: Curso) = {
      c match {
        case x: Curso with t1 => x.a
        case x: Curso with t2 => x.b
      }
    }

    foo(cs1)

  }


}
