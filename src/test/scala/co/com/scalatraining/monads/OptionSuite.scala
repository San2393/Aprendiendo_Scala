package co.com.scalatraining.monads


import org.scalatest.{Documenter, FunSuite}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class OptionSuite extends FunSuite {

  test("Se debe poder crear un Option con valor") {
    val s = Option {
      1
    }
    assert(s == Some(1))
  }

  test("Se debe poder crear un Option para denotar que no hay valor") {
    val s = None
    assert(s == None)
  }

  test("Es inseguro acceder al valor de un Option con get") {
    val s = None
    assertThrows[NoSuchElementException] {
      val r = s.get
    }


  }

  test("Se debe poder hacer pattern match sobre un Option") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)
    var res = ""
    nombre match {
      case Some(nom) => res = nom
      case None => res = "NONAME"
    }
    assert(res == "NONAME")
  }

  test("Se debe poder saber si un Option tiene valor con isDefined") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)
    assert(nombre.isDefined)
  }

  test("Se debe poder acceder al valor de un Option de forma segura con getOrElse") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)
    val res = nombre.getOrElse("NONAME")
    assert(res == "NONAME")
  }

  test("Se debe poder acceder al valor de un Option de forma segura con getOrElse con some") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)
    val res = nombre.getOrElse("NONAME")
    assert(res == "Andres")
  }

  test("Un Option se debe poder transformar con un map") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)
    val nombreCompleto = nombre.map(s => s + " Felipe")
    assert(nombreCompleto.getOrElse("NONAME") == "Andres Felipe")
  }

  test("Un Option se debe poder transformar con un map con None") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(1)
    val nombreCompleto = nombre.map(s => s + " Felipe")
    assert(nombreCompleto.getOrElse("NONAME") == "NONAME")
  }


  test("Un Option se debe poder transformar con flatMap en otro Option") {
    val lista = List(Some("Andres"), None, Some("Luis"), Some("Pedro"))
    val nombre = lista(0)

    val resultado = nombre.flatMap(s => Option(s.toUpperCase))
    resultado.map(s => assert(s == "ANDRES"))
  }

  test("Usando option con flatmap") {
    def foo(i: Int): Option[Int] = {
      Option(i)
    }

    val l = Option(1)
    val res: Option[Int] = l.flatMap(foo)
    val res2: Option[Option[Int]] = l.map(foo)

    assert(res == Some(1))


  }


  test("Usando flatmap ") {
    def foo(i: Int): Option[Int] = {
      Option(i)
    }

    def bar(i: Int): Option[Int] = {
      Option(i)
    }

    def sumar(i: Int, j: Int) = i + j

    val o1 = Option(1)
    val o2 = Option(2)

    val res1 = o1.map(x => o2.map(y => sumar(x, y)))
    val res2 = o1.flatMap(x => o2.map(y => sumar(x, y)))
    val res3 = o1.flatMap(x => o2.flatMap(y => Option(sumar(x, y))))

    assert(res1 == Some(Some(3)))
    assert(res2 == Some(3))
    assert(res3 == Some(3))


  }


  test("Usando future con flatmap ") {
    def foo(i: Int): Future[Int] = Future {
      1
    }

    val l = Option(1)

    assertDoesNotCompile("l.flatMap(foo)")

  }

  test("Option ejemplo evaluando option") {
    def foo(i: Option[Int]): Option[Int] = {
      i
    }

    val l = List(Option(1), None)
    val res = l.map(foo)

    assert(res(0) == Option(1))


  }

  test("Option ejemplo evaluando int") {
    def foo(i: Int): Option[Int] = {
      Option(i)
    }

    val l = Option(1)
    val res = l.map(foo)

    assert(res == Some(Some(1)))


  }

  test("Un Option se debe poder filtrar con una hof con filter") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val option0 = lista(0)
    val option1 = lista(1)
    val option2 = lista(2)
    val res0 = option0.filter(_ > 10)
    val res1 = option1.filter(_ > 10)
    val res2 = option2.filter(_ > 10)

    assert(res0 == None)
    assert(res1 == None)
    assert(res2 == Some(40))
  }

  test("for comprehensions en Option") {
    val lista = List(Some(5), None, Some(40), Some(20))
    val s1 = lista(0)
    val s2 = lista(2)
    val resultado = for {
      x <- s1
      y <- s2
    } yield x + y
    assert(resultado == Some(45))
  }

  test("for comprehesions None en Option") {
    val consultarNombre = Some("Andres")
    val consultarApellido = Some("Estrada")
    val consultarEdad = None
    val consultarSexo = Some("M")

    val resultado = for {
      nom <- consultarNombre
      ape <- consultarApellido
      eda <- consultarEdad
      sex <- consultarSexo
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield (s"$nom $ape, $eda,$sex")

    assert(resultado == None)
  }

  test("for comprehesions None en Option2") {

    def consultarNombre(dni: String): Option[String] = Some("Felix")

    def consultarApellido(dni: String): Option[String] = Some("Vergara")

    def consultarEdad(dni: String): Option[String] = None

    def consultarSexo(dni: String): Option[String] = Some("M")

    val dni = "8027133"
    val resultado = for {
      nom <- consultarNombre(dni)
      ape <- consultarApellido(dni)
      eda <- consultarEdad(dni)
      sex <- consultarSexo(dni)
    //} yield (nom+","+ape+","+eda+","+sex)
    } yield (s"$nom $ape, $eda,$sex")

    assert(resultado == None)
  }


  test("Probando for comprehesions ejemplo de clase") {
    def foo(i: Int): Option[Int] = Option(i * 10)

    def bar(i: Int): Option[Int] = Option(i * 10)

    def suma(x: Int, y: Int) = x + y

    val res = for {
      a <- foo(1)
      b <- bar(2)
    } yield suma(a, b)

    assert(res == Some(30))


  }


  test("Probando for comprehesions ejemplo de clase None") {
    def foo(i: Int): Option[Int] = Option(i * 10)

    val z = None
    val x = Some(4)
    var m = true

    def y: Option[Int] = {
      m = false
      Some(4)
    }

    val res = for {
      a <- x
      b <- z
      c <- y
    } yield a + b + c

    assert(m)


  }


  test("Probando for comprehesions clase 2") {
    def foo(i: Int): Option[Int] = Option(i * 10)

    def bar(i: Int): Option[Int] = Option(i * 10)

    def suma(x: Int, y: Int) = x + y

    val res = for {
      a <- foo(1)
      b <- bar(a)
    } yield suma(a, b)

    assert(res == Some(110))


  }


  test("Probando for comprehesions no compila por usar diferente contexto") {
    def foo(i: Int): Option[Int] = Option(i * 10)

    def bar(i: Int) = i * 10

    def suma(x: Int, y: Int) = x + y

    assertDoesNotCompile("for {a <- foo(1); b <- bar(a)} yield suma(a, b)")

  }

  test("Probando for comprehesions clase 3") {
    def foo(i: Int): Option[Int] = Option(i * 10)

    def bar(i: Option[Int]): Option[Int] = i

    def suma(x: Int, y: Int) = x + y

    val res = for {
      a <- foo(1)
      b <- bar(Option(a))
    } yield suma(a, b)

    assert(res == Some(20))


  }

  /*
  * scala.Option Cheat Sheet
  * */


  test("Ejemplos de flatMap") {
    def foo(i: Int) = Option(i + 1)

    val option = Option(5)

    val a = option match {
      case None => None
      case Some(x) => foo(x)
    }

    assert(option.flatMap(foo) == a)

  }


  test("Ejemplos de flatten") {
    def foo(i: Int) = Option(i)

    val option = Option(5)
    val option2 = Option(Option(5))

    val a = option match {
      case None => None
      case Some(x) => foo(x)
    }

    assert(option2.flatten == a)

  }

  test("Ejemplos de map") {
    def foo(i: Int) = Option(i + 1)

    val option = Option(5)

    val a = option match {
      case None => None
      case Some(x) => Some(foo(x))
    }

    assert(option.map(foo) == a)

  }

  test("Ejemplos de foreach") {
    def foo(i: Int) = Option(i + 1)

    val option = Option(5)

    val a = option match {
      case None => {}
      case Some(x) => foo(x)
    }
    assert(option.foreach(foo) != a)
    //option.foreach(foo) da un Unit

  }


  test("Ejemplos de isDefined") {
    val option = Option(5)

    val a = option match {
      case None => false
      case Some(_) => true
    }

    assert(a == option.isDefined)

  }


  test("Ejemplos de isEmpty") {
    val option = Option(5)

    val a = option match {
      case None => true
      case Some(_) => false
    }

    assert(a == option.isEmpty)

  }

  test("Ejemplos de forall") {
    def foo(i: Int) = Option(i + 1)

    def boo(i: Int) = i > 20

    val option = Option(5)

    val a = option match {
      case None => true
      case Some(x) => foo(x)
    }

    assert(a != option.forall(boo))

  }


  test("Ejemplos de exists") {
    def foo(i: Int) = i == 2

    val option = Option(5)

    val a = option match {
      case None => false
      case Some(x) => foo(x)
    }

    assert(a == option.exists(foo))

  }

  test("Ejemplos de orElse") {
    def foo(i: Option[Int] = Option(0)) = i

    val option = Option(None)

    val a = option match {
      case None => foo()
      case Some(x) => Some(x)
    }

    assert(a == option.orElse(foo()))

  }
  test("Ejemplos de   getOrElse") {
    def foo(i: Option[Int] = Option(0)) = Option(i)

    val option = Option(3)

    val a = option match {
      case None => foo()
      case Some(x) => x
    }

    assert(a == option.getOrElse(foo()))

  }

  test("Ejemplos de toList") {

    val option = Option(3)

    val a = option match {
      case None => Nil
      case Some(x) => x :: Nil
    }

    assert(a == option.toList)

  }

  test("Ejemplos de fold") {

    val option: Option[Int] = Option(3)
    val o: Int = option.fold {
      1
    } { valor => valor + 0 }

    val r = option.getOrElse(Option(3))


    assert(o == r)

  }

  test("Ejemplos de reduce ***** da el mismo valor") {

    val option = Option(1)
    val o = option.reduce(_ + _)


    assert(o == 1)

  }

  test("Promedio de numero en Option") {

    def prom(i: List[Int]): Option[Int] = Option(i.sum / i.size)

    val a = (1 to 100).map(Option(_)).toList
    val m = a.map(x => x.filter(x => x % 2 == 0))
    val list = for {
      x <- m
    } yield x.sum

    val pro = list.filter(x => x != 0)
    val res = prom(pro)
    assert(res == Some(51))

    val vlr2 = a.flatten.filter(x => x % 2 == 0)
    assert(prom(vlr2) == Some(51))


  }

}

