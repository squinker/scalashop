
import common._

import scala.util.Try

package object scalashop {

  val debug = false
  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    // TODO implement using while loops
    // Using radius and x, y , find top left and bottom right
    val xLeftLimit =  clamp(x + radius, 0, (src.width-1))
    val yUpperLimit = clamp(y + radius, 0, (src.height-1))

    val initialXPos = clamp(x - radius, 0, src.height)
    val initialYPos = clamp(y - radius, 0, src.height)

    var xPos: Int = initialXPos
    var yPos: Int = initialYPos

    var reds:   Int = 0
    var greens: Int = 0
    var blues:  Int = 0
    var alphas: Int = 0
    var counter     = 0

    if (debug) { println(s"BB K called with x:$x y:$y radius: $radius") }

    if (debug) { println(s"xLeftLimit:$xLeftLimit  yUpperLimit: $yUpperLimit\n\n\n")}
    while(xPos <= xLeftLimit){
      yPos = initialYPos
      while(yPos <= yUpperLimit) {

        if (debug) { println(s"Trying to get pixel at x: $xPos, y: $yPos, src width is ${src.width} src height is ${src.height}")}
        val pixel = try{
          val pix = src.apply(xPos, yPos)
          if (debug) { println(s"Pixel at $xPos, $yPos is $pix") }

          pix
        }
        catch{
          case e: Throwable => println(e.toString + s" when trying to get pixel at $xPos, $yPos from image ${src.height}")
            throw(e)
        }

        reds   += red(pixel)
        greens += green(pixel)
        blues  += blue(pixel)
        alphas += alpha(pixel)

        counter = counter +1
        yPos += 1
      }
      xPos += 1
    }

    if (debug) { println(s"Totals reds:$reds, greens:$greens, blues:$blues, alphas:$alphas") }

    rgba(
      Try((reds/counter.toFloat).toInt).getOrElse(0),
      Try((greens/counter.toFloat).toInt).getOrElse(0),
      Try((blues/counter.toFloat).toInt).getOrElse(0),
      Try((alphas/counter.toFloat).toInt).getOrElse(0)
    )
  }

}
