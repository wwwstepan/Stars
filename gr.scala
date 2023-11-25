package My.Gr

import scala.swing._
import event._
import java.awt.{ Color, Graphics2D }
import My.Uni._

// Класс, реализующий отрисовку вселенной на элементе диалога "Панель"
class StarsGr() extends Panel {

  var scale:Double = 10.0
  var zeroX=0
  var zeroY=0

  // Событие перерисовки окна, вызывается главным окном
  // при изменении размеров окна а также принудительно в цикле жизни вселенной
  override def paintComponent(g: Graphics2D) {
    g.setBackground(Color.black)
    g.clearRect(0, 0, size.width, size.height)
    
    for(s <- Universe.stars)
      drawStar(g, s)
  }

  // Прорисовка одной звезды
  def drawStar(g: Graphics2D, s:Star): Unit = {
    val szw2 = size.width/2
    val szh2 = size.height/2

    val x = zeroX + (s.par.x*scale + szw2).toInt
    val y = zeroY + (s.par.y*scale + szh2).toInt
    if(x<0 || y<0 || x>size.width-1 || y>size.height-1)
      return

    val (colorOfStar, sizeOfStar) = starColorAnfSize(s)
    g.setColor(colorOfStar)
    sizeOfStar match {
      case 1 => g.drawRect(x,y,1,1)
      case _ => g.fillOval(x-sizeOfStar/2, y-sizeOfStar/2, sizeOfStar, sizeOfStar)
    }
    s match {
      case BlackHole(_) => g.fillOval(x-8, y-1, 17, 3)
      case _ => None
    }
  }

  // В зависимости от типа и возраста звезды возвращает
  // пару значений: цвет и радиус кружка, которы будет обозначена звезда
  def starColorAnfSize(s: Star) = s match {
    case WhiteDwarf(par) => 
    val a = 255 - (if (par.age>15000) 80 else par.age/187)
      (new Color(a, a, a), 1)
    case RedDwarf(par) => 
      val a = 255 - (if (par.age>6000) 50 else par.age/120)
      (new Color(a, 0, 0), 1)
    case YellowDwarf(par) =>
      val a = 255 - (if (par.age>7000) 50 else par.age/120)
      (new Color(a, a, 0), 1)
    case RedGiant(par) =>
      val a = 255 - (if (par.age>140) 70 else par.age/2)
      (new Color(a, 20, 60), 
      if (par.age > 5000) 11 else 2+par.age/500)
    case BlueGiant(par) =>
      val a = 255 - (if (par.age>100) 50 else par.age/2)
      (new Color(0, 0, a), 
      if (par.age > 4000) 9 else 2+par.age/500)
    case NeutronStar(_) =>
      (new Color(70, 70, 70), 1)
    case BlackHole(_) =>
      (new Color(255, 255, 40), 8)
    }
}