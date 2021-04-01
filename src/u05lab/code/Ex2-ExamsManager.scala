package u05lab.code

import u05lab.code.Kind.Kind

/* See: https://bitbucket.org/mviroli/oop2018-esami/src/master/a01b/e1/Test.java */

object Kind extends Enumeration {
  type Kind = Value
  val RETIRED, FAILED, SUCCEEDED = Value
}

trait ExamResult {
  def kind: Kind
  def cumLaude: Boolean
  def evaluation: Option[Int]
}

trait ExamResultFactory {
  def failed: ExamResult

  def retired: ExamResult

  def succeededCumLaude: ExamResult

  def succeeded(evaluation: Int): ExamResult
}

trait ExamsManager {
  def createNewCall(call: String)

  def addStudentResult(call: String, student: String, result: ExamResult)

  def getAllStudentsFromCall(call: String): Set[String]

  def getEvaluationsMapFromCall(call: String): Map[String, Int]

  def getResultsMapFromStudent(student: String): Map[String, Int]

  def getBestResultFromStudent(student: String): Option[Integer]
}

object ExamResult extends ExamResultFactory {
  override def failed: ExamResult = ???

  override def retired: ExamResult = ???

  override def succeededCumLaude: ExamResult = ???

  override def succeeded(evaluation: Int): ExamResult = ???
}

object ExamsManager {
  def apply(): ExamsManager = ExamsManagerImpl
}

private object ExamsManagerImpl extends ExamsManager {
  override def createNewCall(call: String): Unit = ???

  override def addStudentResult(call: String, student: String, result: ExamResult): Unit = ???

  override def getAllStudentsFromCall(call: String): Set[String] = ???

  override def getEvaluationsMapFromCall(call: String): Map[String, Int] = ???

  override def getResultsMapFromStudent(student: String): Map[String, Int] = ???

  override def getBestResultFromStudent(student: String): Option[Integer] = ???
}