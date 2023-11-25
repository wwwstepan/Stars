/*
Главный объект приложения
Окно со сгенерированными звездами
Звезды начинают взаимодействовать друг с другом,
подчиняясь закону всемирного тяготения

Математическая модель не очень точна: при близких
расстояниях и больших скоростях возникают искажения.
Способ борьбы - разбивка больших движений на серию мелких,
достигая приемлемой непрерывности во взаимодействиях,
но данный подход в приложении не реализован.

Кнопки управления видом:
<>^v   -- смещение кадра,
+-     -- увеличение или уменьшение масштаба
Также можно на ходу менять гравитационную постоянную - G+, G-
 */

package My

import swing._
import scala.swing.BorderPanel.Position._
import scala.util.Random
import java.util.Date
import event._
import My.Gr._
import My.Uni._

object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Scala stars"

    preferredSize = new Dimension(1200,900)

    Universe.addRndStars(5)

    val gr = new StarsGr

    // Добавление кнопок на форму
    val panel = new BoxPanel(Orientation.Vertical) {
      contents += Ut.addButton("<"){ gr.zeroX -= 10 }
      contents += Ut.addButton(">"){ gr.zeroX += 10 }
      contents += Ut.addButton("^"){ gr.zeroY -= 10 }
      contents += Ut.addButton("v"){ gr.zeroY += 10 }
      contents += Ut.addButton(" + "){ gr.scale *= 1.5 }
      contents += Ut.addButton(" - "){ gr.scale /= 1.5 }
      contents += Ut.addButton(" G+ "){ Universe.const_G *= 1.22 }
      contents += Ut.addButton(" G- "){ Universe.const_G /= 1.25 }
      contents += Ut.addButton(" Pause "){ isPaused = !isPaused }
    }
	
    val textArea = new TextArea

    contents = new BorderPanel {
      layout(panel) = West
      layout(gr) = Center
      layout(textArea) = South
    }

    var isPaused = false

    Ut.runInThread{
      var i=0
      while(true){
        if (!isPaused) {
          i += 1
          val t1 = Ut.now // замер, сколько микросекунд будет просчитана и нарисована вселенная
          Universe.live
          gr.repaint()
          val t2 = Ut.now
          val dt = Ut.difftime(t2,t1)

          textArea.text = s"$i speed: $dt - ${Universe.stars.length} stars G=${Universe.const_G}"

          val dt_msec = (dt*1000.0).toInt
          // пауза делается, только если все обсчитали менее чем за 50мс
          if (dt_msec<50) Thread.sleep(50-dt_msec)
          //Thread.sleep(500)
        }
      }
    }
  }
}

// Вспомогательные функции
object Ut {

  // Добавление кнопки на форму
  def addButton(caption: String)(onPress: => Unit):Button = {
    new Button{ 
      text=caption
      reactions += { case ButtonClicked(_) => onPress }
    }
  }

  // Вычисления промежутка времени между 2мя моментами в микросекундах
  def difftime(a:Date, b:Date) = (a.getTime().toDouble - b.getTime().toDouble) / 1000.0

  def now = new Date // Функция получения текущего момента времени

  def runInThread(block: => Unit) { // Запуск блока кода в отдельном процессе
    new Thread {
      override def run() { block }
    }.start()
  }
}
