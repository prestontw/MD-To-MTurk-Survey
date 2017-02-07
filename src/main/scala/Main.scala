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
  val IDEAL_REASONING_DELIMITER = '&'

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
      else {
        val text = line.slice(line.indexOf(" ") + 1, line.length)
        if (line(0) == ANSWER_DELIMITER) {
          new QuestionSegment(acc.id,
            acc.answerChoices :+ text,
            acc.text + stringToRadio(acc.id, text))
        }
        else if (line(0) == REASONING_DELIMITER) {
          new QuestionSegment(acc.id, acc.answerChoices, acc.text + stringToForm(acc.id, text))
        }
        else if (line(0) == WANT_DELIMITER) {
          new QuestionSegment(acc.id, acc.answerChoices, acc.text + endPanelBody() + stringToWant(acc.id, text))
        }
        else if (line(0) == CHECKBOX_DELIMITER) {
          new QuestionSegment(acc.id, acc.answerChoices, acc.text + stringToCheckBoxes(acc.id, acc.answerChoices, text))
        }
        else if (line(0) == IDEAL_REASONING_DELIMITER) {
          new QuestionSegment(acc.id, acc.answerChoices, acc.text + stringToIdealExplanation(acc.id, text) + endPanelBody() + endPanel())
        }
        else
          new QuestionSegment(acc.id, acc.answerChoices, acc.text + line + "\n")
      }
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
      |<div class="panel-body"><label>$s </label>\n""".stripMargin
  }
  def endPanelBody(): String = "</div><!-- end panel body -->\n"
  def endPanel(): String = "</div><!-- end panel -->\n"

  def stringToRadio(id: String, value: String): String = {
    s"""<div class="radio"><label><input name="$id" required="" type="radio"
    |\tvalue="$value" /><code>$value</code> </label></div>\n""".stripMargin
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
    |\t<div class="checkbox"><label><input id="${id}ChkBox" name="$id wants different" type="checkbox"
    |\t\t/>Yes, I <i>want</i> it to produce something different.</label></div>
    |</div><!-- end wanting different -->\n""".stripMargin
  }

  def stringToCheckBoxes(id: String, answerChoices: Vector[String], label: String): String = {
    val newId = "Ideal" + id

    s"""<div class="panel-body" id="$newId">
    |\t<label>$label </label>\n""".stripMargin +
    answerChoices.foldLeft("")((acc: String, curr:String) => {
      acc + s"""<div class="checkbox"><label><input name="$newId" type="checkbox"
      |\tvalue=\"$curr" /><code>$curr</code> </label></div>\n""".stripMargin
    }) + "<!-- end checkboxes -->\n"
  }

  def stringToIdealExplanation(id: String, title: String): String = {
    val newId = "Ideal" + id + "Reasoning"

    s"""<div class="form-group">
        |\t<label for="$newId">$title </label>
        |\t<textarea class="form-control" cols="250" id="$newId"
        |\tname="$newId" rows="6"></textarea>\n</div>""".stripMargin
  }
}
