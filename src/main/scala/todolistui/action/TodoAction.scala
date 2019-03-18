package todolistui.action

import todolistui.domain.TodoItem

sealed trait TodoAction

case class UpdateText(text: String) extends TodoAction

case class RemoveTodo(id: Int) extends TodoAction

case class UpdateTodo(id: Int, newText: String) extends TodoAction

case class ToggleTodo(id: Int) extends TodoAction

case class Drop(pred: TodoItem => Boolean) extends TodoAction

case class UpdateFilter(selection: TodoSelection) extends TodoAction

case class EditTodo(id: Int) extends TodoAction

case class EditText(text: String) extends TodoAction

case object AddTodo extends TodoAction

case object AllComplete extends TodoAction

case object SaveEdit extends TodoAction

case object Init extends TodoAction