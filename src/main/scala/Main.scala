package scala.mdtomturk

import java.io._
import scala.io.Source

class QuestionSegment(val id: String, val answerChoices: Vector[String],
                      val text: String) {}

object Main {
  def main(args: Array[String]) = {
    if (args.length < 1) {
      val allargs: String = args.foldLeft("")((acc: String, curr: String) =>
        acc + "|" + curr)
      throw new IOException(s"must specify a file, $allargs")
    }

    val infilename = args(0)
    val outfilename = if (args.length < 2) {
      println("WARNING: using default 'out.html' as name");
      "out.html"
    }
    else args(1)

    val outFileContents: String = Source.fromFile(infilename).getLines().foldLeft(new QuestionSegment("", Vector.empty, ""))(
    (acc: QuestionSegment, line: String) =>
      new QuestionSegment(acc.id, acc.answerChoices, acc.text + line + ".")).text

    val writer = new PrintWriter(new File(outfilename))
    writer.write(outFileContents)
    writer.close()
  }
}
