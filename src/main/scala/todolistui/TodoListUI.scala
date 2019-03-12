package todolistui

import cats.effect.IO
import monix.execution.Scheduler.Implicits.global
import outwatch.dom._
import cats.implicits._
import outwatch.dom.dsl._

object TodoListUI {

  val todoPrograme: IO[VNode] = for {
    addEvents <- Handler.create[String]()
    deleteEvents <- Handler.create[String]()
    additions <- IO(addEvents.map(addToList))
    deletions <- IO(deleteEvents.map(removeFromList))
    state = (additions ++ deletions)
      .scan(List.empty[String])((list, fn) => fn.apply(list))
    listViewsF = state
      .map(_.map(todo => todoComponent(todo, deleteEvents)).sequence)
    listViews <- IO(listViewsF.map(f => f.unsafeRunSync))
    textFieldCom <- textFieldComponent(addEvents)
    todoDiv <- IO(
      div(
        textFieldCom,
        ul(children <-- listViews)
      )
    )
  } yield todoDiv


  def textFieldComponent(outputEvents: Sink[String]): IO[VNode] = {
    for {
      todo <- Handler.create[String]()
      inputDiv <- IO(div(
        label("Todo: "),
        input(onInput.value --> todo),
        button(
          onClick(todo) --> outputEvents,
          "Submit"
        )
      ))
    } yield inputDiv
  }

  def todoComponent(todo: String, deleteEvents: Sink[String]): IO[VNode] = {
    IO(li(
      span(todo),
      button(onClick(todo) --> deleteEvents, "Delete")
    ))
  }

  def addToList(todo: String): List[String] => List[String] = {
    list: List[String] => list :+ todo
  }

  def removeFromList(todo: String): List[String] => List[String] = {
    list: List[String] => list.filterNot(_ == todo)
  }

  def main(args: Array[String]): Unit = {
    val app = for {
      todo <- todoPrograme
      _ <- OutWatch.renderInto("#app", todo)
    } yield ()

    app.unsafeRunSync()
  }
}
