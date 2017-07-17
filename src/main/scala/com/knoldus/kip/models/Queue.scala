package com.knoldus.kip.models

package com.knoldus.kip.models


class Queue(queueWithList: List[Int]) {

  def enqueue(x: Int): Queue = {
    val newQueue = x :: queueWithList.reverse
    new Queue(newQueue.reverse)
  }

  def dequeue: Queue = {
    new Queue(queueWithList.tail)
  }
}
