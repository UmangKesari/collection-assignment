package com.knoldus.kip.models


package com.knoldus.kip.models

class Stack(stackWithList: List[Int]) {

  def pop: Stack = {
    if(stackWithList.isEmpty) {
      throw new RuntimeException
    }
    new Stack(stackWithList.tail)
  }

  def push(x: Int): Stack = {
    new Stack(x :: stackWithList)
  }

}