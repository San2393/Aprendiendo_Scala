package co.com.scalatraining.collections

import org.scalatest.FunSuite

import scala.collection.immutable.Queue


/**
  * Created by janus on 8/02/17.
  */
class QueueSuite extends FunSuite {


  test("Primer Queue con reverse") {
    val que = Queue(1, 2, 3)
    assert(que.reverse == Queue(3, 2, 1))
  }

  test("Uso de Queue con lista") {
    val que = Queue(List(1, 2), List(4, 3))
    val ls = que.filter(valor => valor.head > 2)

    assert(ls.head.head == 4)

  }

  test("Probando Queue con fold") {
    var i = 0
    val que = Queue("Hola", "Que")
    val l = que.fold("") {
      (Acumu, item) =>
        i = i + 1
        Acumu + i + item
    }

    assert(l == "1Hola2Que")
  }

  test("actualizando valor ") {
    val que = Queue(9, 5, 1, 2)
    val que2 = que.updated(0, 6)
    assert(que2.head == 6)
    que.dequeue
  }


}
