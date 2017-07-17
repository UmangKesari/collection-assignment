package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase

import com.knoldus.kip.models.{Gender, ScoreCard, Student}
import RamDatabase._

trait CollectionAssignment {



  def getScorecardsByName(name: String): List[ScoreCard] = {

    val map = generateScorecards

    map(name) match {
      case x: ScoreCard => List(x)
      case x: List[ScoreCard] => x.sortBy(_.studentId)
      case _ => throw new Exception("Name not found")
    }
  }
  def getStudentMarks(id:Long) : Map[Long,Float]  = {
    RamDatabase.marksList.filter(_.studentId == id).groupBy(_.subjectId).map{case(k,v)=>(k.toLong,v.head.marksObtained)}
  }
  def getPercentage(id:Long) : Float = {
    val listOfMarks = RamDatabase.marksList.filter(_.studentId == id)
    listOfMarks.foldLeft(0.toFloat)((x,y)=> x + y.marksObtained)/listOfMarks.size
  }

  //Collection Based - Assignment 1
  def generateScorecards: Map[String, AnyRef] = {

    def computeMarks(id: Long): Map[Long, Float] = {
      val marks = marksList.filter(id == _.studentId)

      marks.map(x => x.subjectId.toLong -> x.marksObtained).toMap[Long, Float]
    }

    def calculatePercentage(id: Long) = {
      val marks: List[Float] = marksList.filter(id == _.studentId).map(_.marksObtained)
      marks.sum / marks.length
    }

    def computeScoreCard(student: List[Student]): List[ScoreCard] = {
      for (s <- student)
        yield new ScoreCard(s.id, computeMarks(s.id), calculatePercentage(s.id))
    }

    def compute(list: List[Student], map: Map[String, AnyRef]): Map[String, AnyRef] = {

      if (list.isEmpty) {
        map
      }
      else if (!list.tail.exists(_.name == list.head.name)) {

        val newMap = map + (list.head.name ->
          ScoreCard(list.head.id, computeMarks(list.head.id), calculatePercentage(list.head.id)))
        compute(list.tail, newMap)

      }
      else {
        val newMap = map + (list.head.name -> computeScoreCard(list.head :: list.tail.filter(_.name == list.head.name)))
        compute(list.tail.filter(_.name != list.head.name), newMap)

      }

    }
    compute(studentList, Map[String, AnyRef]())

  }

  //Collection Based - Assignment 2
  def getScoreCardByGender: (List[ScoreCard], List[ScoreCard]) = {

    val (maleStudents: List[Student], femaleStudents: List[Student]) = RamDatabase.studentList.partition(_.gender == Gender.MALE)

    val maleScorecards = maleStudents.map(_.name).distinct.flatMap(x=>getScorecardsByName(x))

    val femaleScorecards = femaleStudents.map(_.name).distinct.flatMap(x=>getScorecardsByName(x))

    (maleStudents.map((student)=> ScoreCard(student.id,getStudentMarks(student.id),getPercentage(student.id))),
      femaleStudents.map((student)=> ScoreCard(student.id,getStudentMarks(student.id),getPercentage(student.id))))

  }

  def getScoreCardsWithHigherPercentage: (List[ScoreCard], List[ScoreCard]) = {

    val (maleScoreCard,femaleScoreCard)  =  getScoreCardByGender

    (maleScoreCard.filter(_.percentage >= 50) , femaleScoreCard.filter(_.percentage >= 50) )

  }

  def getSimilarPercentageBwGroups: List[((String, ScoreCard), (String, ScoreCard))] = {

    def getStudentName(st_Id : Long):String = studentList.find(_.id==st_Id).get.name

    val (maleScoreCard,femaleScoreCard)  =  getScoreCardByGender

    maleScoreCard.flatMap(x => femaleScoreCard.filter(_.percentage==x.percentage).map(y=>
      ((getStudentName(x.studentId),x),(getStudentName(y.studentId),y))))
  }

  def femalePercentageNotInMales: List[(String, ScoreCard)] = {
    def getStudentName(st_Id : Long):String = studentList.find(_.id==st_Id).get.name
    val (mScoreCard,fScoreCard)  =  getScoreCardByGender


    for( fs <- fScoreCard if !mScoreCard.exists(_.percentage==fs.percentage))
      yield (getStudentName(fs.studentId), fs)
  }

}