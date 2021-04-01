package u05lab

import org.junit.jupiter.api.Assertions.{assertEquals, assertFalse, assertThrows, assertTrue}
import org.junit.jupiter.api.Test
import u05lab.code.ExamResult._
import u05lab.code._

class ExamsManagerTest {

  @Test
  def testExamResultsBasicBehaviour() { // esame fallito, non c'è voto
    assertEquals(failed.kind, Kind.FAILED)
    assertFalse(failed.evaluation.isDefined)
    assertFalse(failed.cumLaude)
    assertEquals(failed.toString, "FAILED")
    // lo studente si è ritirato, non c'è voto
    assertEquals(retired.kind, Kind.RETIRED)
    assertFalse(retired.evaluation.isDefined)
    assertFalse(retired.cumLaude)
    assertEquals(retired.toString, "RETIRED")
    // 30L
    assertEquals(succeededCumLaude.kind, Kind.SUCCEEDED)
    assertEquals(succeededCumLaude.evaluation, Some(30))
    assertTrue(succeededCumLaude.cumLaude)
    assertEquals(succeededCumLaude.toString, "SUCCEEDED(30L)")
    // esame superato, ma non con lode
    assertEquals(succeeded(28).kind, Kind.SUCCEEDED)
    assertEquals(succeeded(28).evaluation, Some(28))
    assertFalse(succeeded(28).cumLaude)
    assertEquals(succeeded(28).toString, "SUCCEEDED(28)")
  }

  // verifica eccezione in ExamResultFactory
  @Test
  def optionalTestEvaluationCantBeGreaterThan30()  {
    assertThrows(classOf[IllegalArgumentException], () => {
      succeeded(32)
    })
  }

  @Test
  def optionalTestEvaluationCantBeSmallerThan18() {
    assertThrows(classOf[IllegalArgumentException], () => {
      succeeded(17)
    })
  }

  // metodo di creazione di una situazione di risultati in 3 appelli
  private def prepareExams(): ExamsManager = {
    val em = ExamsManager()
    em.createNewCall("gennaio")
    em.createNewCall("febbraio")
    em.createNewCall("marzo")
    em.addStudentResult("gennaio", "rossi", failed) // rossi -> fallito

    em.addStudentResult("gennaio", "bianchi", retired) // bianchi -> ritirato

    em.addStudentResult("gennaio", "verdi", succeeded(28)) // verdi -> 28

    em.addStudentResult("gennaio", "neri", succeededCumLaude) // neri -> 30L

    em.addStudentResult("febbraio", "rossi", failed) // etc..

    em.addStudentResult("febbraio", "bianchi", succeeded(20))
    em.addStudentResult("febbraio", "verdi", succeeded(30))
    em.addStudentResult("marzo", "rossi", succeeded(25))
    em.addStudentResult("marzo", "bianchi", succeeded(25))
    em.addStudentResult("marzo", "viola", failed)
    em
  }

  // verifica base della parte obbligatoria di ExamManager
  @Test def testExamsManagement() {
    val em = prepareExams()
    // partecipanti agli appelli di gennaio e marzo
    assertEquals(Set("rossi", "bianchi", "verdi", "neri"), em.getAllStudentsFromCall("gennaio"))
    assertEquals(Set("rossi", "bianchi", "viola"), em.getAllStudentsFromCall("marzo"))
    // promossi di gennaio con voto
    assertEquals(em.getEvaluationsMapFromCall("gennaio").size, 2)
    assertEquals(em.getEvaluationsMapFromCall("gennaio")("verdi"), 28)
    assertEquals(em.getEvaluationsMapFromCall("gennaio")("neri"), 30)
    // promossi di febbraio con voto
    assertEquals(em.getEvaluationsMapFromCall("febbraio").size, 2)
    assertEquals(em.getEvaluationsMapFromCall("febbraio")("bianchi"), 20)
    assertEquals(em.getEvaluationsMapFromCall("febbraio")("verdi"), 30)
    // tutti i risultati di rossi (attenzione ai toString!!)
    assertEquals(em.getResultsMapFromStudent("rossi").size, 3)
    assertEquals(em.getResultsMapFromStudent("rossi")("gennaio"), "FAILED")
    assertEquals(em.getResultsMapFromStudent("rossi")("febbraio"), "FAILED")
    assertEquals(em.getResultsMapFromStudent("rossi")("marzo"), "SUCCEEDED(25)")
    // tutti i risultati di bianchi
    assertEquals(em.getResultsMapFromStudent("bianchi").size, 3)
    assertEquals(em.getResultsMapFromStudent("bianchi")("gennaio"), "RETIRED")
    assertEquals(em.getResultsMapFromStudent("bianchi")("febbraio"), "SUCCEEDED(20)")
    assertEquals(em.getResultsMapFromStudent("bianchi")("marzo"), "SUCCEEDED(25)")
    // tutti i risultati di neri
    assertEquals(em.getResultsMapFromStudent("neri").size, 1)
    assertEquals(em.getResultsMapFromStudent("neri")("gennaio"), "SUCCEEDED(30L)")
  }

  // verifica del metodo ExamManager.getBestResultFromStudent
  @Test def optionalTestExamsManagement() {
    val em = prepareExams()
    // miglior voto acquisito da ogni studente, o vuoto..
    assertEquals(em.getBestResultFromStudent("rossi"), Some(25))
    assertEquals(em.getBestResultFromStudent("bianchi"), Some(25))
    assertEquals(em.getBestResultFromStudent("neri"), Some(30))
    assertEquals(em.getBestResultFromStudent("viola"), None)
  }


  @Test
  def optionalTestCantCreateACallTwice() {
    val em = prepareExams()
    assertThrows(classOf[IllegalArgumentException], () => {
      em.createNewCall("marzo")
    })
  }

  @Test
  def optionalTestCantRegisterAnEvaluationTwice() {
    val em = prepareExams()
    assertThrows(classOf[IllegalArgumentException], () => {
      em.addStudentResult("gennaio", "verdi", failed)
    })
  }
}
