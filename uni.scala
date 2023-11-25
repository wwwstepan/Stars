package My.Uni

import scala.collection.mutable._
import scala.util.Random
import math._





case class StarParam(var x:Double, var y:Double, var mass:Double, var dx:Double, var dy:Double, var age:Int, var i: Int)

abstract class Star(val par: StarParam) {
  def live {
    par.x += par.dx
    par.y += par.dy
    par.age += 1
  }
}
case class RedDwarf(override val par: StarParam) extends Star(par)
case class YellowDwarf(override val par: StarParam) extends Star(par)
case class WhiteDwarf(override val par: StarParam) extends Star(par)
case class RedGiant(override val par: StarParam) extends Star(par)
case class BlueGiant(override val par: StarParam) extends Star(par)
case class NeutronStar(override val par: StarParam) extends Star(par)
case class BlackHole(override val par: StarParam) extends Star(par)

// Объект "Вселенная", содержит массив звезд
object Universe {

  val stars = new ArrayBuffer[Star](0)

  var nDivs=0

  def Add(x:Double, y:Double, mass:Double) = {
    val startAge = Random.nextInt(5000)
    stars +=  (Random.nextDouble()*1.0 match {
      case a if(a < StarType.WhiteDwarf) =>
        WhiteDwarf(StarParam(x, y, mass, 0, 0, startAge, StarIndex.getIndex))
      case a if(a < StarType.RedDwarf) =>
        RedDwarf(StarParam(x, y, mass, 0, 0, startAge, StarIndex.getIndex))
      case a if(a < StarType.YellowDwarf) =>
        YellowDwarf(StarParam(x, y, mass, 0, 0, startAge, StarIndex.getIndex))
      case a if(a < StarType.RedGiant) =>
        RedGiant(StarParam(x, y, mass, 0, 0, startAge, StarIndex.getIndex))
      case a if(a < StarType.BlueGiant) =>
        BlueGiant(StarParam(x, y, mass, 0, 0, startAge, StarIndex.getIndex))
      case a if(a < StarType.NeutronStar) =>
        NeutronStar(StarParam(x, y, mass, 0, 0, startAge, StarIndex.getIndex))
      case _ =>
        BlackHole(StarParam(x, y, mass*10.0, 0, 0, startAge, StarIndex.getIndex))
    })
  }

  def addRndStar = {
    val r = Random.nextDouble()*30.0
    val ang = Random.nextDouble()*2.0*math.Pi
    val x = math.cos(ang)*r
    val y = math.sin(ang)*r
    val mass = 1.0 + Random.nextDouble()*bigStarMass
    Add(x,y,mass)
  }

  def addRndStars(numOfStars: Int) = {
    for(i <- 1 to numOfStars) 
      addRndStar
  }

  // Шаг жизни вселенной, сначала между каждой парой звезд
  // вычисляется сила притяжения, потом все звезды смещаются
  // на величину, зависящую от суммарной силы
  def live = { 
    /*try {
      for(s <- stars if (abs(s.x)>500.0 || abs(s.y)>500.0)){ stars -= s }
    } catch { case _:Throwable => None}*/
    gravity
    move 
  }

  def gravity = {
    for(s1 <- stars.par; s2 <- stars.par)
      gravity2Stars(s1, s2, 1)
  }

  // Вычислется сила притяжения между двумя звездами
  // выраженная как смещения звезд по осям x и y
  def gravity2Stars(s1: Star, s2:Star, difCoef: Int): Unit = {
    
    if (difCoef > 1e5 || s1.par.i <= s2.par.i)
      return

    val dist_x = s1.par.x - s2.par.x
    val dist_y = s1.par.y - s2.par.y
  
    if (abs(dist_x)<0.00001 && abs(dist_y)<0.00001)
      return

    val dist2 = dist_x * dist_x + dist_y * dist_y
    val dist = sqrt(dist2)
    
    val f = Universe.const_G * s2.par.mass / dist2

    var difCoefDbl = difCoef.toDouble
    var ddx = f * dist_x / (dist * difCoefDbl)
    var ddy = f * dist_y / (dist * difCoefDbl)
println(f"${s1.par.i}=${s2.par.i} $difCoefDbl%.2f (${s1.par.x}%.2f, ${s1.par.y}%.2f) (${s2.par.x}%.2f, ${s2.par.y}%.2f)  dist=$dist%.3f  ddx $ddx%.5f  ddy $ddy%.5f  [${s1.par.dx}%.5f  ${s1.par.dy}%.5f][${s2.par.dx}%.5f  ${s2.par.dy}%.5f]")
    val minD = 0.03
    if (dist > 2.0 || (ddx < minD && ddy < minD &&
       abs(s1.par.dx) < minD && abs(s1.par.dy) < minD &&
       abs(s2.par.dx) < minD && abs(s2.par.dy) < minD)
    ) {
      s1.par.dx -= ddx
      s1.par.dy -= ddy
      s2.par.dx += ddx
      s2.par.dy += ddy
      return
    }
    println("***")

    if (difCoef != 1) {
      s1.par.dx = -ddx; s1.par.dy = -ddy
      s2.par.dx = ddx; s2.par.dy = ddy
      return
    }

    val newDifCoef = (myMax(abs(ddx), abs(ddy), abs(s1.par.dx), 
      abs(s1.par.dy), abs(s2.par.dx), abs(s2.par.dy)) / minD).toInt + 2
    
    DoDifGravity(s1, s2, newDifCoef)

    s1.par.dx = 0; s1.par.dy = 0
    s2.par.dx = 0; s2.par.dy = 0
  }

  def myMax(args: Double*): Double = {
    var max = 0.0
    for (arg <- args)
      if (max < arg)
        max = arg
    max
  }

  def DoDifGravity(s1: Star, s2:Star, newDifCoef: Int) {
    for (i <- 0 to newDifCoef) {
println(f"###  ${s1.par.i}=${s2.par.i} $newDifCoef%1.5f")
      gravity2Stars(s1, s2, newDifCoef)
      s1.par.x += s1.par.dx
      s1.par.y += s1.par.dy
      s2.par.x += s2.par.dx
      s2.par.y += s2.par.dy
    }
  }

  def move = {
    for(s <- stars)
      s.live
  }

  var const_G = 5e-4
  val bigStarMass = 500.0
}
