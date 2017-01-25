package scala.mdtomturk

import java.io._
import scala.io.Source

class QuestionSegment(val id: String, val answerChoices: Vector[String],
                      val text: String) {}

object Main {
  val ID_DELIMITER = '!'
  val ANSWER_DELIMITER = '-'
  val REASONING_DELIMITER = '?'
  val WANT_DELIMITER = '#'
  val CHECKBOX_DELIMITER = '.'

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
    (acc: QuestionSegment, line: String) => {
      if (line.length == 0)
        new QuestionSegment(acc.id, acc.answerChoices, acc.text + "\n")
      else if (line(0) == ID_DELIMITER) {
        // then we need to update id, and add section header thingy
        val lineInfo: (String, String) = parseIDLine(line)
        new QuestionSegment(lineInfo._1, Vector.empty,
          acc.text + stringToPanel(lineInfo._2))
      }
      else if (line(0) == ANSWER_DELIMITER) {
        val answerText = line.slice(line.indexOf(" ") + 1, line.length)
        new QuestionSegment(acc.id,
          acc.answerChoices :+ answerText,
          acc.text + stringToRadio(acc.id, answerText))
      }
      else if (line(0) == REASONING_DELIMITER) {
        val sectionText = line.slice(line.indexOf(" ") + 1, line.length)
        new QuestionSegment(acc.id, acc.answerChoices, acc.text + stringToForm(acc.id, sectionText))
      }
      else if (line(0) == WANT_DELIMITER) {
        val sectionText = line.slice(line.indexOf(" ") + 1, line.length)
        new QuestionSegment(acc.id, acc.answerChoices, acc.text + endPanelBody() + stringToWant(acc.id, sectionText))
      }
      else if (line(0) == CHECKBOX_DELIMITER) {
        val sectionText = line.slice(line.indexOf(" ") + 1, line.length)
        new QuestionSegment(acc.id, acc.answerChoices, acc.text + stringToCheckBoxes(acc.id, acc.answerChoices, sectionText))
      }
      else
        new QuestionSegment(acc.id, acc.answerChoices, acc.text + line + ".")
    }).text

    val writer = new PrintWriter(new File(outfilename))
    writer.write(outFileContents)
    writer.close()
  }

  def parseIDLine(l: String): (String, String) = {
    val spaceIndex: Int = l.indexOf(" ")
    (l.slice(1, spaceIndex), l.slice(spaceIndex + 1, l.length))
  }

  def stringToPanel(s: String): String = {
    s"""<div class="panel panel-default">
      |<div class="panel-body"><label>$s </label>
      |<pre><code> </code></pre>\n\n""".stripMargin
  }
  def endPanelBody(): String = "</div><!-- end panel body -->\n"
  def endPanel(): String = "</div><!-- end panel -->\n"

  def stringToRadio(id: String, value: String): String = {
    s"""<div class="radio"><label><input name="$id" required="" type="radio"
    |\tvalue="$value" />$value </label></div>\n""".stripMargin
  }

  def stringToForm(id: String, title: String): String = {
    val newId = id + "Reasoning"
    s"""<div class="form-group">
    |\t<label for="$newId">$title </label>
    |\t<textarea class="form-control" cols="250" id="$newId"
    |\tname="$newId" rows="6" required=""></textarea>\n</div>\n""".stripMargin
  }

  def stringToWant(id: String, label: String): String = {
    s"""<div class="panel-body">
    |\t<label>$label </label>
    |\t<div class="checkbox"><label><input id="!!!" name="$id wants different" type="checkbox"
    |\t\t/>Yes, I <i>want</i> it to produce something different.</label></div>
    |</div><!-- end wanting different -->\n""".stripMargin
  }

  def stringToCheckBoxes(id: String, answerChoices: Vector[String], label: String): String = {
    val newId = "Ideal" + id
    s"""<div class="panel-body" id="$newId">
    |\t<label>$label </label>\n""".stripMargin +
    answerChoices.foldLeft("")((acc: String, curr:String) => {
      acc + s"""<div class="checkbox"><label><input name="$newId" type="checkbox"
      |\tvalue=\"$curr" />$curr </label></div>\n""".stripMargin
    }) + "</div><!-- end checkboxes -->\n"
  }
}
