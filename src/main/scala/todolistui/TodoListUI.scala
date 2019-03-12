package todolistui

import outwatch.dom._
import outwatch.dom.dsl._
import monix.execution.Scheduler.Implicits.global

object TodoListUI {
  def main(args: Array[String]): Unit = {

    OutWatch.renderInto("#app", h1("Hello World")).unsafeRunSync()
  }
}
