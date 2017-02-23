package scalashop

import common._
import org.scalameter._

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    * starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    // TODO implement this method using the `boxBlurKernel` method

    if (debug) {
      println(s"Vertical bb called with from: $from, end: $end for source image of height ${src.height} , width ${src.width}")
    }
    if (debug) {
      println(s"Destination image has height ${dst.height} and width ${dst.width}")
    }

    var curRow = from
    var curCol = 0

    while (curRow < end) {
      curCol = 0
      while (curCol < src.width) {

        val blurredRGBA = boxBlurKernel(src, curCol, curRow, radius)
        dst.update(curCol, curRow, blurredRGBA)
        curCol += 1
      }
      curRow += 1
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * rows.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val starts = (0 to src.height).by(numTasks).toList
    starts.zip(starts.tail).map { startEnd =>
      val t = task(blur(src, dst, startEnd._1, startEnd._2, radius))
      t.join
    }
  }

  /*
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    // TODO implement using the `task` construct and the `blur` method
    val starts = (0 to src.width).by(numTasks).toList
    starts.zip(starts.tail).map { startEnd =>
      val t = task( blur(src, dst, startEnd._1, startEnd._2, radius) )
      t.join
    }
  }

   */

}
