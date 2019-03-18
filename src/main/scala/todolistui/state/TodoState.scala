package todolistui.state

import cats.effect.IO
import monix.execution.Scheduler
import outwatch.ProHandler
import outwatch.util.Store
import todolistui.action._
import todolistui.domain.TodoItem

final case class TodoState(
                            nextId: Int,
                            todos: List[TodoItem],
                            text: String,
                            todoSelection: TodoSelection,
                            editingId: Option[Int],
                            editText: String,
                          )

object TodoState {
  type TodoStore = ProHandler[TodoAction, (TodoAction, TodoState)]
  type SubStore[A] = ProHandler[TodoAction, A]
  type AppReducer = Store.Reducer[TodoAction, TodoState]
  val AppReducer: Store.Reducer.type = Store.Reducer

  def appStore(implicit S: Scheduler): IO[TodoStore] =
    Store.create(Init, TodoState(0, Nil, "", TodoSelection.All, None, ""), appReducer)

  def appReducer: AppReducer = AppReducer.justState(reducer)

  def reducer(state: TodoState, action: TodoAction): TodoState = action match {
    case AddTodo => state.copy(
      nextId = state.nextId + 1,
      todos = TodoItem(state.nextId, state.text, completed = false) :: state.todos,
      text = ""
    )
    case UpdateText(text) => state.copy(text = text)
    case RemoveTodo(id) => state.copy(
      todos = state.todos.filterNot(_.id == id)
    )
    case UpdateTodo(id, newText) => updateById(id, state)(_.copy(title = newText))
    case ToggleTodo(id) => updateById(id, state)(x => x.copy(completed = !x.completed))
    case AllComplete => state.copy(todos = state.todos.map(_.copy(completed = true)))
    case Drop(pred) => state.copy(todos = state.todos.filterNot(pred))
    case UpdateFilter(todoSelection: TodoSelection) => state.copy(todoSelection = todoSelection)
    case EditTodo(id: Int) => state.copy(
      editingId = Some(id),
      editText = state.todos.find(_.id == id).map(_.title).getOrElse(""),
    )
    case EditText(text: String) => state.copy(editText = text)
    case SaveEdit => state.editingId.map(id =>
      updateById(id, state)(_.copy(title = state.editText)).copy(
        editingId = None,
        editText = ""
      )
    ).getOrElse(state)
    case Init => state
  }

  private def updateById(id: Int, state: TodoState)(update: TodoItem => TodoItem): TodoState = {
    state.todos.find(_.id == id).map { todo =>
      state.copy(todos = update(todo) :: state.todos.filterNot(_.id == id))
    }.getOrElse(state)
  }
}
