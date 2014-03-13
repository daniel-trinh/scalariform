package scalariform.gui

import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent

object SwingUtils {
  implicit class Fn2ListSelectionListener(handler: ListSelectionEvent ⇒ Unit) extends ListSelectionListener {
    def valueChanged(e: ListSelectionEvent) = handler(e)
  }
}