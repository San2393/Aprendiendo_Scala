package co.com.scalatraining.syntax

import org.scalatest.FunSuite

class SyntaxSuite extends FunSuite {

  test("Un var debe permitir realizar asignaciones") {
    var x = 0
    assert(x == 0)
    x = 2
    assert(x == 2)
  }

  test("Un val no debe permitir realizar asignaciones") {
    val x = 0
    assert(x == 0)
    assertDoesNotCompile("x = 1")
  }

  test("Los tipos en Scala son iferidos por el compilador") {
    // Fijate como no hay que decir de qué tipo es x
    val x = 0
    assert(x == 0)

    // Aunque tambien lo puedes hacer explicito si asi lo quieres
    val y = "0"
    assert(y == "0")

    // Si eres incredulo fijate como el tipo es fuerte y no debil
    var strong = "0"

    assertDoesNotCompile("strong = 1")
  }

  test("Scala no debe permitir iniciar en null") {
    var x = null
    assertDoesNotCompile("x = 1")
  }

  test("Scala no debe permitir declarar sin asignar") {
    assertDoesNotCompile("var x")
  }

  test("Un object puede tener funciones miembro") {

    object obj {

      var x = 1
      val y = 0

      def f1(a: Int, b: Int): Int = {
        x = x + 1
        a + x
      }

      def f2(a: Int) = {
        a + 2
      }

      def esPar(x: Int): Boolean = {
        x % 2 == 0

      }
    }

    //fijate como no hay que hacer new de obj
    val res = obj.f2(1)
    obj.x = 3

    assert(res == 3)
    assert(obj.x == 3)
    assert(obj.y == 0)
    assert(obj.esPar(2))
  }

  test("Un class se puede comoportar como un class tradicional") {

    //los parametros de contruccion se definen entre parentesis a continuacion del nombre de la clase
    class MyClass(a: Int) {
      def f1 = a + 1

      def f2 = a + 2

    }

    // A una class se le debe instanciar con new pasándole los atributos que define para su construccion
    val mc = new MyClass(1)
    val res = mc.f1
    assert(res == 2)
  }

  test("A un class se le puede  mutar su estado") {

    //los parametros de contruccion se definen entre parentesis a continuacion del nombre de la clase
    class MyClass(a: Int) {

      var r = 0

      def f1 = {
        r = r + 2
        a + 1
      }

      def f2 = a + 2

      def geta = a
    }

    // A una class se le debe instanciar con new pasándole los atributos que define para su construccion
    val mc = new MyClass(1)

    assert(mc.geta == 1)
    assert(mc.r == 0)
    val res1 = mc.f1
    assert(mc.r == 2)
    val res2 = mc.f1
    assert(mc.r == 4)
  }

  test("Clase sin atributos") {
    class Clase() {
      var n = 1

      def f1 = n + 1
    }

    val cls = new Clase()
//    println(s"Mensaje clase   ${cls} ")
    assert(cls.n == 1)
  }

  test("Un case es una clase normal para usos especificos") {

    case class MyCaseClass(a: Int, b: Int) {
      def f1(a: Int) = a + 1
    }

    case class MyCaseClass2(var a: Int, b: Int) {
      def f1(a: Int) = a + 1
    }

    // Se puede instanciar de forma normal
    val mcc1 = new MyCaseClass(1, 2)
    assert(mcc1.f1(1) == 2)

    // Se puede instanciar sin new
    val mcc2 = MyCaseClass(1, 2)
    assert(mcc2.f1(1) == 2)

    //Que pasa si intentamos println(mcc2) ?
//    println(s"Mensaje clase case ${mcc1} ")


    // Pregunta cuáles son esos casos específicos
    var cc2 = MyCaseClass2(1, 2)
    assert(cc2.a == 1)
    cc2.a = 5
    assert(cc2.a == 5)


  }

  test("Un trait puede tener solo definiciones") {
    trait MyTrait {
      def f1(a: Int): Boolean
    }

    trait MySecondTrait {
      def f2(a: String): Int
    }

    class MyClass extends MyTrait with MySecondTrait {
      override def f1(a: Int) = ???

      override def f2(a: String) = ???
    }

    assertThrows[NotImplementedError] {
      val mc = new MyClass
      mc.f1(1)
    }

  }

  test("Un trait puede tener tambien implementaciones") {
    trait MyTrait {
      def f1(a: Int) = a + 1
    }

    object MyObject extends MyTrait


    class MyClass extends MyTrait




    val mo = MyObject.f1(1)
    val mc = new MyClass
    val res = mc.f1(1)
    assert(res == 2)
    assert(mo == 2)
  }


  test("Realizando mezclas de trait") {

    trait Am {
      val NumeroArrugas = 0
    }

    trait Cost {
      val NumeroArrugas = 40
    }

    object MyObject extends Am with Cost {
      override val NumeroArrugas = 20
    }

    val mo = MyObject.NumeroArrugas

    assert(mo == 20)

  }


  test("Probando objetos") {

    object PrimerObjecto {
      def resta(a: Int, b: Int): Int = {
        a - b
      }

      def multiplicar(a: Int, b: Int): Int = {
        a * b
      }
    }

    val r = PrimerObjecto.resta(9, 2)
    val m = PrimerObjecto.multiplicar(1, 2)

    assert(r == 7)
    assert(m == 2)


  }

  test("Probando clases") {

    class PrimeraClase(x: Int) {

      def f1(y: Int, z: Int): Int = {
        x + y - z
      }

    }
    val pc = new PrimeraClase(2)
    val f1 = pc.f1(3, 5)
    assert(f1 == 0)


  }


  test("Probando clases case") {

    case class PrimeraClaseCase(x: Int, var a: Int) {

      def f1(y: Int, z: Int): Int = {
        a = a + 1
        x + y - z
      }

    }
    val pc = PrimeraClaseCase(7, 2)
    val f1 = pc.f1(3, 5)
    assert(f1 == 5)
    assert(pc.a == 3)


  }


  test("Probando trait") {
    trait Joven {
      val edad = 20

    }

    trait Viajero {
      val nacionalidad = "Colombiano"
    }

    trait Estudiando {
      val semestre = 9
    }


    class Persona extends Joven with Viajero with Estudiando

    val prs = new Persona

    assert(prs.edad == 20)
    assert(prs.nacionalidad == "Colombiano")
    assert(prs.semestre == 9)


  }


  test("Probando la union de trait, case, class, object") {
    trait Casa extends Barrio with Ciudad {
      val cuartos = 3
      val tamaño = "grande"
    }

    trait Barrio {
      val nombre = "belen"
    }

    trait Ciudad {
      var numero = 4
    }
    case class Ubicacion(var a: Int) {

      object MiCasa extends Casa {
        override val cuartos: Int = 2

        def cambioCiudad = {
          numero = a
        }

      }

    }

    val mc = Ubicacion(5)
    assert(mc.MiCasa.numero == 4)
    mc.MiCasa.cambioCiudad
    assert(mc.MiCasa.numero == 5)
  }


  test("Probando mas trait") {
    trait Primero {
      def f1(x: Int): Int = {
        x * 5
      }
    }

    trait Segundo {
      def f1(x: Int): Int = {
        x * 10
      }
    }

    class Seleccion extends Primero with Segundo {
      override def f1(x: Int): Int = super[Primero].f1(x)
    }

    val s = new Seleccion
    val f1 = s.f1(5)
    assert(f1 == 25)


    //mas trait

    trait Gato {
      val color: String
    }

    trait Especie {
      val nombre: String
    }

    object Animal extends Gato with Especie {
      val color = "Negro"
      val nombre = "Felino"
    }

    assert(Animal.color == "Negro")
    assert(Animal.nombre == "Felino")


  }


  test("Prueba con trait ") {
    trait Primero {
      val color = "Rojo"

      def caminar = println("Caminar en el primero")

      def correr = println("correr en el primero")
    }



    trait Segundo extends Primero {
      override def caminar = println("Caminar en el segundo")

      override val color = "Amarillo"

    }

    object Ultimo extends Segundo {
      override def correr = println("correr en el ultimo")

      override val color = "Azul"
    }

    assert(Ultimo.color == "Azul")


  }

  test("Ejemplo trait") {
    class MyClass(i: Int, s: String)

    trait a {
      val foo = "Hola"
      val numero = 1
    }

    val mc = new MyClass(1, "d") with a

    assert(mc.foo == "Hola")
    assert(mc.numero == 1)

  }

}
