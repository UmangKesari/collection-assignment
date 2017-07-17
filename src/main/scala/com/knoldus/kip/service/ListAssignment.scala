package com.knoldus.kip.service

  import com.knoldus.kip.models.Student
  import com.knoldus.kip.RamDatabase._

  trait ListAssignment {

    //Assignment -1
    def failedStudents(subjectId: Long, percentage: Double, passOrFail: String): Int = {
      passOrFail match {
        case "pass" => marksList.count(x => (x.subjectId == subjectId && x.marksObtained >= percentage))
        case "fail" => marksList.count(x => (x.subjectId == subjectId && x.marksObtained < percentage))
      }
    }

    def topBottomStudents(subjectId: Long, count: Int, topOrBottom: String): List[Student] = {
      topOrBottom match {
        case "top" => {
          val topMarksList = marksList.filter(_.subjectId == subjectId).sortBy(_.marksObtained).reverse.
            take(count)
          studentList.filter(_.id == topMarksList.map(_.studentId))
        }
        case "botttom" => {
          val bottomMarksList = marksList.filter(_.subjectId == subjectId).sortBy(_.marksObtained).
            take(count)
          studentList.filter(_.id == bottomMarksList.map(_.studentId))
        }
      }
    }

    def topAndLeastScorers(topOrBottom: String, count: Int): List[Student] = {
      topOrBottom match {
        case "top" => {
          val studentIdWithMarks = studentList.map(_.id).
            zip(studentList.map(x => marksList.filter(x.id == _.studentId).
              map(_.marksObtained).sum)).sortBy(_._2).reverse
          val topStudents = studentIdWithMarks.take(count)
          val topStudentIds = topStudents.map(_._1)
          studentList.filter(topStudentIds contains _.id)
        }
        case "bottom" => {
          val studentIdWithMarks = studentList.map(_.id).zip(studentList.map(x => marksList.filter(x.id == _.studentId).map(_.marksObtained).sum)).sortBy(_._2)
          val bottomStudents = studentIdWithMarks.take(count)
          val bottomStudentIds = bottomStudents.map(_._1)
          studentList.filter(bottomStudentIds contains _.id)
        }
      }
    }

    def getScholarshipGroups(percentage: Float, goodScholarship: Int, normalScholarship: Int)
    : (List[(Student, Int)], List[(Student, Int)]) = {

      val inputPercentage = percentage * 5
      val studentIdWithMarks = studentList.map(_.id).
        zip(studentList.map(x => marksList.filter(x.id == _.studentId).
          map(_.marksObtained).sum))

      val (studentIdWithGoodScholarship, studentIdWithNormalScholarship) = studentIdWithMarks.
        partition(_._2 >= inputPercentage)

      val studentWithGoodScholarship = studentList.filter(studentIdWithGoodScholarship.
        map(_._1) contains _.id)

      val studentWithNormalScholarship = studentList.filter(studentIdWithNormalScholarship.
        map(_._1) contains _.id)

      val listOfGoodScholarship = List.fill(studentWithGoodScholarship.length)(goodScholarship)
      val listOfNormalScholarship = List.fill(studentWithNormalScholarship.length)(normalScholarship)
      val zippedListGoodScholarship = studentWithGoodScholarship zip listOfGoodScholarship
      val zippedListNormalScholarship = studentWithNormalScholarship zip listOfNormalScholarship
      (zippedListGoodScholarship, zippedListNormalScholarship)
    }

    def passedOrFailed(passOrFail: String, percentage: Float): List[Student] = {
      val studentIdWithMarks = studentList.map(_.id).
        zip(studentList.map(x => marksList.filter(x.id == _.studentId).
          map(_.marksObtained).sum))
      val inputPercentage = percentage * 5

      passOrFail match {
        case "pass" => {
          val filteredStudentWithMarks = studentIdWithMarks.filter(_._2 >= inputPercentage)
          studentList.filter(filteredStudentWithMarks.map(_._1) contains _.id)
        }
        case "fail" => {
          val filteredStudentWithMarks = studentIdWithMarks.filter(_._2 < inputPercentage)
          studentList.filter(filteredStudentWithMarks.map(_._1) contains _.id)
        }
      }
    }

    def studentsWithMoreThan95: List[Student] = {
      val studentIdWithMarks = studentList.map(_.id).
        zip(studentList.map(x => marksList.filter(x.id == _.studentId).
          map(_.marksObtained).sum))
      val filteredStudentWithMarks = studentIdWithMarks.filter(_._2 >= 475)
      studentList.filter(filteredStudentWithMarks.map(_._1) contains _.id)
    }

    def generateReport: List[(String, List[Float])] = {
      val listOfListOfMarks = marksList.groupBy(_.studentId).values.map(_.map(_.marksObtained))

      val setOfKeys = marksList.groupBy(_.studentId).mapValues(_.map(_.marksObtained)).keys

      val listOfListOfNames = for {
        l <- setOfKeys.toList
        listOfIndividualNames = studentList.filter(_.id == l).map(_.name)
      } yield listOfIndividualNames
      val listOfNames = listOfListOfNames.flatten
      listOfNames zip listOfListOfMarks
    } //Must use the groupBy() Method of the List

    //Assignment - 2
    def getLastElementWithIndex(list: List[String]): (String, Int) = {
      def lastElementWithIndex(list: List[String], str: String, index: Int): (String, Int) = {
        if (list.isEmpty) {
          (str, index)
        }
        else {
          val str1 = list.head
          lastElementWithIndex(list.tail, str1, index + 1)
        }
      }

      lastElementWithIndex(list, str = "", index = 0)
    }

    def printTable(list: List[Long]): List[Long] = {
        val result=for
        {
          base<-0 to list.size
          toYield = for {
            index<- 1 to 10
          }yield list(base) * index
        }yield toYield.toList
        val result1=result.toList
        val result2=result1.flatMap(_ map(_*1))
        result2
    }

    def aggregateLists(list1: List[String], list2: List[Long]): List[List[(String, Long)]] = {
      val zipList = list1 zip list2
      zipList.map(t => List[(String, Long)]((t._1, t._2)))

    }

    def getSumOfList(list: List[Long]): Long = {
      def sum(acc: Long, list: List[Long]): Long = {
        if (list.isEmpty) {
          acc
        }
        else {
          val sumAcc = acc + list.head
          sum(sumAcc, list.tail)
        }
      }

      sum(0, list)
    }

    def getMultiplicationOfList(list: List[Long]): Long = {
      def product(acc: Long, list: List[Long]): Long = {
        if (list.isEmpty) {
          acc
        }
        else {
          val prodAcc = acc * list.head
          product(prodAcc, list.tail)
        }
      }

      product(1, list)
    }

    def quickSortList(list: List[Long]): List[Long] = {

      list match {
        case x :: Nil => list
        case x :: ys => {
          val pivot = x
          quickSortList(ys filter (pivot > _)) ::: List(pivot) ::: quickSortList(ys filter (pivot < _))
        }
        case Nil => Nil
      }

      def mergeSortList(list: List[Long]): List[Long] = {
        val n = list.length / 2
        if (n == 0) list // i.e. if list is empty or single value, no sorting needed
        else {
          val (left, right) = list.splitAt(n)
          merge(mergeSortList(left), mergeSortList(right))
        }

        def merge(left: List[Long], right: List[Long]): List[Long] ={
          (left, right) match {
            case (left, Nil) => left
            case (Nil, right) => right
            case (leftHead :: leftTail, rightHead :: rightTail) =>
              if (leftHead < rightHead) leftHead :: merge(leftTail, right)
              else rightHead :: merge(left, rightTail)
          }

      }
    }
  }
}
