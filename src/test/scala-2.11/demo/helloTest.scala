package demo

import org.scalatest.FunSuite

class helloTest extends FunSuite {

  test("it should say hello to xianning") {
    val demo = new Hello
    assert(demo.sayHello("xianning") == "hello, xianning")
  }

}