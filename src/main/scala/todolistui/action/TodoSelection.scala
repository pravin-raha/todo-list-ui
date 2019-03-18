package todolistui.action

import todolistui.domain.TodoItem

sealed abstract class TodoSelection(val name: String) extends Product with Serializable {
  def url: String = s"#/$name"

  def predicate(t: TodoItem): Boolean
}

object TodoSelection {

  def selections: List[TodoSelection] = All :: Active :: Completed :: Nil

  case object All extends TodoSelection("all") {
    def predicate(t: TodoItem) = true
  }

  case object Active extends TodoSelection("active") {
    def predicate(t: TodoItem): Boolean = !t.completed
  }

  case object Completed extends TodoSelection("completed") {
    def predicate(t: TodoItem): Boolean = t.completed
  }

}
