package u05lab.code

import u05lab.code.Kind.Kind

/* See: https://bitbucket.org/mviroli/oop2018-esami/src/master/a01b/e1/Test.java */

object Kind extends Enumeration {
  type Kind = Value
  val RETIRED, FAILED, SUCCEEDED = Value
}

trait ExamResult {
  val kind: Kind
  val cumLaude: Boolean
  val evaluation: Option[Int]
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

  def getResultsMapFromStudent(student: String): Map[String, String]

  def getBestResultFromStudent(student: String): Option[Int]
}

private case class ExamResultImpl(
                                   override val kind: Kind,
                                   override val cumLaude: Boolean,
                                   override val evaluation: Option[Int]
                                 ) extends ExamResult {
  {
    require((kind == Kind.SUCCEEDED && evaluation.isDefined && 18 <= evaluation.get && evaluation.get <= 30)
      || (kind == Kind.RETIRED && evaluation.isEmpty && !cumLaude)
      || (kind == Kind.FAILED && (evaluation.isEmpty || (evaluation.isDefined && evaluation.get < 18 && evaluation.get >= 0)) && !cumLaude)
    )
  }
  override def toString: String = kind.toString +
    (if (evaluation.isDefined) "(" + evaluation.get + (if (cumLaude) "L" else "") + ")" else "")
}

object ExamResult extends ExamResultFactory {
  override def failed: ExamResult = ExamResultImpl(Kind.FAILED, false, None)

  override def retired: ExamResult = ExamResultImpl(Kind.RETIRED, false, None)

  override def succeededCumLaude: ExamResult = ExamResultImpl(Kind.SUCCEEDED, true, Some(30))

  override def succeeded(evaluation: Int): ExamResult = ExamResultImpl(Kind.SUCCEEDED, false, Some(evaluation))
}


object ExamsManager {
  def apply(): ExamsManager = new ExamsManagerImpl
}

private class ExamsManagerImpl extends ExamsManager {
  private var map: Map[String, Map[String, ExamResult]] = Map()

  override def createNewCall(call: String): Unit = {
    require(!map.contains(call))
    map = map ++ Map(call -> Map())
  }

  override def addStudentResult(call: String, student: String, result: ExamResult): Unit = {
    require(map.contains(call))
    require(!map(call).contains(student))
    map = map ++ Map(call -> (map(call) ++ Map(student -> result)))
  }

  override def getAllStudentsFromCall(call: String): Set[String] = map(call).keySet

  override def getEvaluationsMapFromCall(call: String): Map[String, Int] = map(call)
      .filter(_._2.evaluation.isDefined)
      .map(e => (e._1, e._2.evaluation.get))

  override def getResultsMapFromStudent(student: String): Map[String, String] = map
    .filter(_._2.contains(student))
    .map(e => (e._1, e._2(student).toString))

  override def getBestResultFromStudent(student: String): Option[Int] = map
    .values
    .filter(_.contains(student))
    .map(_(student))
    .filter(_.evaluation.isDefined)
    .map(_.evaluation.get)
    .maxOption
}