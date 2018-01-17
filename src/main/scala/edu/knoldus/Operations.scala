package edu.knoldus

object Operations {

  def length[A](l: List[A]): Int = {
    l.foldRight(0)((_, count: Int) => count + 1)
  }

  def hasSubSequence[A](list: List[A], sub: List[A]): Boolean = {

    def innerSub(inputList: List[A], subList: List[A]): Boolean = {
      subList match {
        case Nil => false
        case head :: tail => inputList match {
          case Nil => false
          case inputHead :: inputTail if inputHead == head => innerSub(inputTail,tail)
          case _ :: inputTail => innerSub(inputTail, sub)
        }
      }
    }

    innerSub(list, sub)
  }

  def concatList[A](l1: List[A], l2: List[A]): List[A] = {

    def concatElement(outputList: List[A], secondList: List[A]): List[A] = {
      secondList match {
        case Nil => outputList
        case head :: tail => concatElement(outputList :+ head,tail)
      }
    }

    if (l1.isEmpty) {l2}
    else if (l2.isEmpty){ l1}
    else {concatElement(l1,l2)}
  }

  def splitList[A](l: List[A], function: A => Boolean): (List[A], List[A]) = {

    def innerSplit(inputList: List[A], falseList: List[A], trueList: List[A]): (List[A], List[A]) = {
      inputList match {
        case Nil => (falseList, trueList)
        case head :: tail if function(head) => innerSplit(tail, falseList, trueList :+ head)
        case head :: tail => innerSplit(tail, falseList :+ head, trueList)
      }
    }
    innerSplit(l, List.empty[A], List.empty[A])
  }


}
