package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 64
    println(s"Radius: $radius, numTasks: $numTasks")
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {
  val debug = false;
  /** Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method

    if (debug) {println(s"Vertical bb called with from: $from, end: $end for source image of height ${src.height} , width ${src.width}") }
    if (debug) {println(s"Destination image has height ${dst.height} and width ${dst.width}")}

    var curCol = from
    var curRow = 0

     while(curCol < end){
       curRow = 0
       while(curRow < src.height){

         val blurredRGBA = boxBlurKernel(src, curCol, curRow, radius)
         try {
           dst.update((curCol), curRow, blurredRGBA)
          }
         catch{
           case e: Throwable => println(s"Errored trying to update img (height:${dst.height}, width: ${dst.width} at curCol:$curCol, curRow:$curRow")
             throw e
         }
         curRow += 1
       }
       curCol += 1
     }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val starts = (0 to src.width).by(numTasks).toList
    starts.zip(starts.tail).map { startEnd =>
      val t = task( blur(src, dst, startEnd._1, startEnd._2, radius) )
      t.join
    }
  }


}
