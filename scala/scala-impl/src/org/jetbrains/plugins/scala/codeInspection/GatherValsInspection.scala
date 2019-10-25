package org.jetbrains.plugins.scala.codeInspection

import java.io.{BufferedWriter, File, FileWriter}

import com.intellij.codeInspection.{InspectionManager, LocalQuickFix, ProblemDescriptor, ProblemHighlightType}
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.lang.psi.api.statements.{ScPatternDefinition, ScVariableDefinition}

import scala.collection.mutable

class GatherValsInspection extends AbstractRegisteredInspection {


  override protected def problemDescriptor(element: PsiElement,
                                           maybeQuickFix: Option[LocalQuickFix] = None,
                                           descriptionTemplate: String = getDisplayName,
                                           highlightType: ProblemHighlightType = ProblemHighlightType.GENERIC_ERROR_OR_WARNING)
                                          (implicit manager: InspectionManager, isOnTheFly: Boolean): Option[ProblemDescriptor] = {

    val result = element match {
      case value: ScVariableDefinition if value.isSimple =>
        val e = value.declaredElements.head
        for (t <- e.`type`().toOption if !t.isUnit && !t.isNothing && !t.isAny && !t.isAnyRef && !t.isAnyVal; expr <- value.expr) yield (e.name, t.presentableText, expr)
        /*.foreach(t =>
          println(e.name + ": " + t.presentableText + " = " + )
        )*/

      case value: ScPatternDefinition if value.isSimple =>
        val e = value.declaredElements.head
        for (t <- e.`type`().toOption if !t.isUnit && !t.isNothing && !t.isAny && !t.isAnyRef && !t.isAnyVal; expr <- value.expr) yield (e.name, t.presentableText, expr)
      case _ =>
        None
    }

    result.foreach {
      case (name, t, expr) =>
        val text = removeWS(expr.getText) + "\t" + removeWS(t) + "\t" + name
        GatherValsInspection.add(text)
    }

    None
  }

  private def removeWS(text: String): String = {
    var oldText = null: String
    var newText = text
    while (oldText != newText) {
      oldText = newText
      newText = newText
        .replaceAll("( ( )+)|\t|\n|\r", " ")
    }
    newText
  }
}

object GatherValsInspection {
  def add(text: String): Unit = GatherValsInspection.synchronized {
    if(found.add(text)) {
      file.write(text)
      file.newLine()
      if (found.size % 100 == 0) {
        println("Already found " + found.size)
        println("Just added: " + text)
      }
    }
  }
  private val file = {
    val file = new File(System.getProperty("user.home") + File.separator + "all_vals")
    new BufferedWriter(new FileWriter(file))
  }

  private val found = mutable.HashSet.empty[String]
}