package todolistui

import cats.effect.IO
import monix.reactive.Observable
import outwatch.dom._
import outwatch.dom.dsl._
import org.scalajs.dom.KeyboardEvent
import outwatch.dom.helpers.EmitterBuilder
import todolistui.state.TodoState
import monix.execution.Scheduler.Implicits.global
import todolistui.action._
import todolistui.state.TodoState.TodoStore


object TodoListUI {

  val enter: Int = 13
  val onEnterUp: EmitterBuilder[KeyboardEvent, VDomModifier] = onKeyUp.transform(_.filter(_.keyCode == enter))

  val todoInput: IO[BasicVNode] = TodoState.appStore map {
    implicit store =>
      div(
        addTodo(),
        todoList()
      )
  }

  def addTodo()(implicit store: TodoStore): BasicVNode =
    input(cls := "new-todo",
      placeholder := "What needs to be done?",
      autoFocus,
      value <-- store.map(_._2.text),
      onInput.target.value.map(UpdateText) --> store,
      onEnterUp.mapTo(AddTodo) --> store
    )

  def todoList()(implicit store: TodoStore): Observable[BasicVNode] = {
    store.map { case (_, state) =>
      val todos = state.todos.filter(state.todoSelection.predicate).sortBy(-_.id)
      ul((cls := "todo-list") ::
        todos.map { todo =>
          state.editingId.filter(_ == todo.id).map { _ =>
            input(cls := "edit",
              defaultValue := todo.title,
              onInput.target.value.map(EditText) --> store,
              onEnterUp.mapTo(SaveEdit) --> store,
              onBlur.mapTo(SaveEdit) --> store,
              autoFocus,
            )
          }.getOrElse {
            li(
              input(
                cls := "toggle",
                tpe := "checkbox",
                checked := todo.completed,
                onInput.map(_ => ToggleTodo(todo.id)) --> store,
              ),
              label(todo.title, styleAttr := (if (todo.completed) "text-decoration: line-through;" else "")),
              onDblClick.mapTo(EditTodo(todo.id)) --> store,
            )
          }
        }: _*
      )
    }
  }

  def main(args: Array[String]): Unit = {
    val app = for {
      todo <- todoInput
      _ <- OutWatch.renderInto("#app", todo)
    } yield ()

    app.unsafeRunSync()
  }

}
