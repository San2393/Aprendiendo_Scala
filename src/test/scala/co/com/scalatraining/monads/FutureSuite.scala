package co.com.scalatraining.monads

import java.util.concurrent.Executors

import org.scalatest.FunSuite

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.never
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class FutureSuite extends FunSuite {

  test("Un futuro se puede crear") {

    val hiloPpal = Thread.currentThread().getName

    var hiloFuture = ""
    val saludo = Future {
      hiloFuture = Thread.currentThread().getName
      Thread.sleep(500)
      "Hola"
    }
    val resultado = Await.result(saludo, 10 seconds)
    assert(resultado == "Hola")
    assert(hiloPpal != hiloFuture)
  }

  test("map en Future") {


    val saludo = Future {

      Thread.sleep(500)
      "Hola"
    }
    val saludoCompleto = saludo.map(mensaje => {
      mensaje + " muchachos"
    })
    val resultado = Await.result(saludoCompleto, 10 seconds)
    assert(resultado == "Hola muchachos")
  }

  test("Se debe poder encadenar Future con for-comp") {
    val f1 = Future {
      Thread.sleep(200)
      1
    }

    val f2 = Future {
      Thread.sleep(200)
      2
    }

    val f3 = for {
      res1 <- f1
      res2 <- f2
    } yield res1 + res2

    val res = Await.result(f3, 10 seconds)

    assert(res == 3)

  }

  test("Se debe poder manejar el error de un Future de forma imperativa") {
    val divisionCero = Future {
      Thread.sleep(100)
      10 / 0
    }
    var error = false

    val r = divisionCero.onFailure {
      case e: Exception => error = true
    }

    Thread.sleep(1000)

    assert(error == true)
  }

  test("Se debe poder manejar el exito de un Future de forma imperativa") {

    val division = Future {
      5
    }

    var r = 0

    val f = division.onComplete {
      case Success(res) => r = res
      case Failure(e) => r = 1
    }

    Thread.sleep(150)

    val res = Await.result(division, 10 seconds)

    assert(r == 5)
  }

  test("Se debe poder manejar el error de un Future de forma funcional sincronicamente") {

    var threadName1 = ""
    var threadName2 = ""

    val divisionPorCero = Future {
      threadName1 = Thread.currentThread().getName
      Thread.sleep(100)
      10 / 0
    }.recover {
      case e: ArithmeticException => {
        threadName2 = Thread.currentThread().getName
        "No es posible dividir por cero"
      }
    }

    val res = Await.result(divisionPorCero, 10 seconds)

    assert(threadName1 == threadName2)
    assert(res == "No es posible dividir por cero")

  }

  test("Se debe poder manejar el error de un Future de forma funcional asincronamente") {

    var threadName1 = ""
    var threadName2 = ""

    implicit val ecParaPrimerHilo = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

    val f1 = Future {
      threadName1 = Thread.currentThread().getName
      2 / 0
    }(ecParaPrimerHilo)
      .recoverWith {
        case e: ArithmeticException => {

          implicit val ecParaRecuperacion = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

          Future {
            threadName2 = Thread.currentThread().getName
            1
          }(ecParaRecuperacion)
        }
      }

    val res = Await.result(f1, 10 seconds)

    //    println(threadName1)
    //    println(threadName2)

    assert(threadName1 != threadName2)
  }

  test("Los future **iniciados** fuera de un for-comp deben iniciar al mismo tiempo") {

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val additionalTime = 50D

    val estimatedElapsed = (Math.max(Math.max(timeForf1, timeForf2), timeForf3) + additionalTime) / 1000

    val f1 = Future {
      Thread.sleep(timeForf1)
      1
    }
    val f2 = Future {
      Thread.sleep(timeForf2)
      2
    }
    val f3 = Future {
      Thread.sleep(timeForf3)
      3
    }

    val t1 = System.nanoTime()

    val resultado = for {
      a <- f1
      b <- f2
      c <- f3
    } yield (a + b + c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed <= estimatedElapsed)
    assert(res == 6)

  }

  test("Los future **definidos** fuera de un for-comp deben iniciar secuencialmente") {

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val estimatedElapsed = (timeForf1 + timeForf2 + timeForf3) / 1000

    def f1 = Future {
      Thread.sleep(timeForf1)
      1
    }

    def f2 = Future {
      Thread.sleep(timeForf2)
      2
    }

    def f3 = Future {
      Thread.sleep(timeForf3)
      3
    }

    val t1 = System.nanoTime()

    val resultado = for {
      a <- f1
      b <- f2
      c <- f3
    } yield (a + b + c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed >= estimatedElapsed)
    assert(res == 6)

  }

  test("Los future declarados dentro de un for-comp deben iniciar secuencialmente") {

    val t1 = System.nanoTime()

    val timeForf1 = 100
    val timeForf2 = 100
    val timeForf3 = 100

    val estimatedElapsed = (timeForf1 + timeForf2 + timeForf3) / 1000

    val resultado = for {
      a <- Future {
        Thread.sleep(timeForf1)
        1
      }
      b <- Future {
        Thread.sleep(timeForf2)
        2
      }
      c <- Future {
        Thread.sleep(timeForf3)
        3
      }
    } yield (a + b + c)

    val res = Await.result(resultado, 10 seconds)
    val elapsed = (System.nanoTime() - t1) / 1.0E09

    assert(elapsed >= estimatedElapsed)
    assert(res == 6)
  }

  /*
  * Aprendiendo a usar Future
  * */


  test("Usando filter con recover") {
    val a = Future {
      5 / 0
    }.recover {
      case e: Exception => {
        0
      }
    }

    val m = a.filter(x => x > 5).recover {
      case e: Exception => {
        9
      }
    }
    assert(Await.result(m, 10 second) == 9)

  }


  test("Usando lo Future con Try") {

    val b = Future(10 / 0)

    val a = b.map(x => Try(x - 1)).recover {
      case e: Exception => {
        0
      }
    }

    assert(Await.result(a, 10 seconds) == 0)

  }

  test("Ejemplo de Future capturar el mayor de las listas") {

    val lis = List(5, 7, 34, 36)
    val lis2 = List(Some(8), None, Some(9))

    def foo(l: List[Int], l2: List[Option[Int]]) = {

      Future {
        def mayor(v: Int, v2: Int): Int = {
          v > v2 match {
            case true => v
            case false => v2
          }
        }

        val li = l.zip(l2)

        li.map(x => Option(mayor(x._2.getOrElse(0), x._1)))
      }
    }

    assert(Await.result(foo(lis, lis2), 10 second) == List(Some(8), Some(7), Some(34)))

  }


  test("Usando onSuccess en dos puntos") {
    var m = ""
    var M = ""

    val text = Future {
      "M" * 16 + "m" * 3
    }
    text onSuccess {
      case txt => m += txt.count(_ == 'm')
    }

    text onSuccess {
      case txt => M += txt.count(_ == 'M')
    }

    Thread.sleep(500)
    assert(m == "3")
    assert(M == "16")

    Await.result(text, 10 seconds)


  }


  test("Futuro que no termina lanza TimeoutException con el tiempo que lo esperes") {
    def a = {
      Future {
        while (true) 8
      }
    }

    assertDoesNotCompile("Await.result(a, 10 seconds)")

  }

  test("Future con Currying") {
    def foo(x: Int => Int)(m: Int => Int) = {

      Future {
        x(6) + m(5)
      }.recover {
        case e: Exception => {
          0
        }
      }

    }

    def zoo(m: Int) = {
      (m - m) / 0
    }

    def num(x: Int) = {
      x * 10
    }

    val a = foo(zoo) _

    val o = a(num)
    assert(Await.result(o, 10 seconds) == 0)


  }


  test("Probando ExecutionContext") {


    def a = Future {
      Thread.sleep(500)
      4
    }

    implicit val ecParaPrimerHilo = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

    def b = Future {
      Thread.sleep(500)
      4
    }(ecParaPrimerHilo)


    val m = a
    val n = b


    val resultado = for {
      x <- a
      y <- b

    } yield y + x

    val res = Await.result(resultado, 10 seconds)

    assert(res == 8)


  }


  test("Probando for-comp") {
    def a = Future("Ho")

    def b(m: String) = Future(m + "La")

    val mm = for {
      x <- a
      y <- b(x)
    } yield y

    assert(Await.result(mm, 10 second) == "HoLa")

  }


  test("Probando dos ExecutionContext") {

    implicit val Hilo1 = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
    implicit val Hilo2 = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))
    var threadName1 = ""
    var threadName2 = ""


    def factorial(n: Int): Int = n match {
      case 0 => 1
      case _ => n * factorial(n - 1)
    }


    def a = Future {
      threadName1 = Thread.currentThread().getName
      factorial(7)
    }(Hilo1)

    def b = Future {
      threadName2 = Thread.currentThread().getName

      factorial(5)
    }(Hilo2)

    assert(Await.result(a, 10 seconds) == 5040)
    assert(Await.result(b, 10 seconds) == 120)
    assert(threadName1 != threadName2)

  }


  /*
  * Probando ejemplos de la pagina de https://github.com/viktorklang/blog
*/



  test("Usando flatten") {

    def n = {
      Future {
        7
      }
    }

    val a = Future {
      n
    }

    val b = Await.result(Await.result(a, 10 second), 10 seconds)

    assert(b == Await.result(a.flatten, 10 seconds))

  }

  test("Usando Zip para obtener una tupla de Future") {

    val n = Future {
      7
    }


    val a = Future {
      "Hola"
    }

    val b: Future[(Int, String)] = n zip a


    assert(Await.result(b, 10 seconds) == (7, "Hola"))


  }


  test("Usando transform en Future") {
    val m = Future {
      "NO"
    }

    val n = Future {
      55 / 0
    }
    val a = m.transform(Try(_))
    val b = n.transform(Try(_))

    assert(Await.result(a, 10 seconds) == Success("NO"))

    assertDoesNotCompile("Await.result(b, 10 seconds) == Failure(java.lang.ArithmeticException)")
  }


  test("Usando foreach") {
    var r = 0

    def a(i: Int) = {
      i
    }

    def b(i: Int) = {
      a(i * 2)

    }

    val c = Future(1)
    c.foreach(x => r = 1)

    Thread.sleep(500)

    assert(r == 1)

    // Future Unit probando
    val al = Future {
      4
    }

    val oo: never.type = Future.never


    def foo(s: String): Future[Unit] = s match {
      case _ => Future.unit
    }

    val f: Future[String] = Future {
      "h"
    }
    val f2 = f flatMap foo


    assert(Await.result(f2, 10 seconds) == ())
  }


  test("Probando transform en clase") {

    val f = Future {
      "Hola1".toInt
    }

    val mm = f.transform(_ match {
      case Success(l) => Try(l)
      case Failure(a) => Try(a)
    })

    assertDoesNotCompile("Await.result(mm, 10 seconds)")
  }


  test("Probando transformWith en ejemplo de clase") {

    val f = Future(1 / 0)
    val f2 = f.transformWith(x => Future(Try(40.2)))

    assert(Await.result(f2, 10 seconds) == Success(40.2))
  }


}