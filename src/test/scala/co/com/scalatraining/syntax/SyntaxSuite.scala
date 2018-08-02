package co.com.scalatraining.syntax

import org.scalatest.{Assertion, FunSuite}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

class SyntaxSuite extends FunSuite{

  test("Un var debe permitir realizar asignaciones"){
    var x = 0
    assert(x == 0)
    x = 2
    assert(x == 2)
  }

  test("Un val no debe permitir realizar asignaciones"){
    val x = 0
    assert(x == 0)
    assertDoesNotCompile("x = 1")
  }

  test("Los tipos en Scala son iferidos por el compilador"){
    // Fijate como no hay que decir de qué tipo es x
    val x = 0
    assert(x == 0)

    // Aunque tambien lo puedes hacer explicito si asi lo quieres
    val y= "0"
    assert(y == "0")

    // Si eres incredulo fijate como el tipo es fuerte y no debil
    var strong = "0"

    assertDoesNotCompile ("strong = 1")
  }

  test("Scala no debe permitir iniciar en null"){
    var x: Null = null
    assertDoesNotCompile("x = 1")
  }

  test("Scala no debe permitir declarar sin asignar"){
    assertDoesNotCompile("var x")
  }

  test("Scala si debe permitir declarar sin asignar pero con tipo"){
    assertDoesNotCompile("var x:Int")
  }

  test("Un object puede tener funciones miembro"){

    object obj {

      var x = 1
      val y = 0

      def f1(a: Int, b:Int):Int = {
        x = x + 1
        a + x
      }

      def f2(a: Int): Int = {
        a + 2
      }
    }

    //fijate como no hay que hacer new de obj
    val res = obj.f2(1)

    assert(res == 3)
  }

  test("Un class se puede comoportar como un class tradicional"){

    //los parametros de contruccion se definen entre parentesis a continuacion del nombre de la clase
    class MyClass(a:Int){
      def f1 = a + 1
      def f2 = a + 2

    }

    // A una class se le debe instanciar con new pasándole los atributos que define para su construccion
    val mc: MyClass = new MyClass(1)
    val res = mc.f1
    assert(res == 2)

    val mc2 = new MyClass(1)
    assert(mc != mc2)

  }

  test("A un class se le puede  mutar su estado"){

    //los parametros de contruccion se definen entre parentesis a continuacion del nombre de la clase
    class MyClass(a:Int){

      var r = 0

      def f1 = {
        r = r + 2
        a + 1
      }

      def f2 = a + 2
    }

    // A una class se le debe instanciar con new pasándole los atributos que define para su construccion
    val mc = new MyClass(1)

    println(s"Imprimiendo una clase normal (no case): ${mc}")

    assert(mc.r == 0)
    val res1 = mc.f1
    assert(mc.r == 2)
    val res2 = mc.f1
    assert(mc.r == 4)
  }

  test("Un case es una clase normal para usos especificos"){

    case class MyCaseClass(a:Int, b:Int) {
      def f1(a:Int) = a + 1
    }

    // Se puede instanciar de forma normal
    val mcc1 = new MyCaseClass(1, 2)
    assert(mcc1.f1(1) == 2)

    // Se puede instanciar sin new
    val mcc2 = MyCaseClass(1,2)
    println(s"mcc: ${mcc2}")

    assert(mcc2.f1(1) == 2)

    //Que pasa si intentamos println(mcc2) ?
    println("Imprimiendo una clase class " + mcc2)

    // Pregunta cuáles son esos casos específicos

  }

  test("Un trait puede tener solo definiciones"){
    trait MyTrait {
      def f1(a:Int):Boolean
    }

    trait MySecondTrait{
      def f2(a:String):Int
    }

    class MyClass extends MyTrait with MySecondTrait{
      override def f1(a:Int) = ???
      override def f2(a:String) = ???
    }

    assertThrows[NotImplementedError]{
      val mc = new MyClass
      mc.f1(1)
    }

  }

  test("Un trait puede tener tambien implementaciones"){
    trait MyTrait {
      def f1(a:Int) = a + 1
    }

    class MyClass extends MyTrait

    val mc = new MyClass
    val res = mc.f1(1)
    assert(res == 2)
  }

  test("class vs case class"){
    class MyClass(a:Int)
    case class MyCaseClass(a:Int)

    val c1 = new MyClass(1)
    val c2 = new MyClass(1)
    val cc1 =  MyCaseClass(1)
    val cc2 =  MyCaseClass(1)

    assert(c1 != c2)
    assert(cc1 == cc2)

  }


  test("Pattern matching"){
    case class Profesor(nombre:String)
    case class Curso(nombre:String, p:Profesor)

    val c1 = Curso("Scala", Profesor("JP"))

    val res = c1 match {
      case x:Curso if x.p.nombre != "JP"=> {
        assert(x.nombre=="Scala")
        assert(x.p==Profesor("JP"))

      }
      case Curso(x,y) if y.nombre == "JP" => {
        println("Pasa?")
        assert(x=="Scala")
        assert(y==Profesor("JP"))

      }
    }

  }


  test("verificacion de unapply"){
    class Profesor(nombre:String, edad:Int)

    object Profesor{
        def unapply(arg: Profesor): Option[(String,Int)] = Some( ("NOMBRE-FIJO",0) )
    }

    new Profesor("JP", 33) match{
      case Profesor(n, e) => {
        assert(n != "JP")
        assert(n=="NOMBRE-FIJO")
        assert(e == 0)
      }
    }

  }

}
