package scala99Solutions

object oneStars {
  def main(args: Array[String]) {

  }

  def getLast[T](ourList: List[T]): T = ourList match {
    case current :: List() => current
    case current :: rest => getLast(rest)
  }

  def getBeforeLast[T](ourList: List[T]): T = ourList match {
    case current :: List() => current
    case current :: rest =>
      if (rest.tail.isEmpty) {
        current
      } else {
        getBeforeLast(rest)
      }
  }

  def getNth[T](nth: Int, ourList: List[T]): T = ourList match {
    case current :: List() => current
    case current :: rest =>
      if (nth == 0) {
        current
      } else {
        getNth(nth - 1, rest)
      }
  }

  def getSize[T](ourList: List[T]): Int = {
    def startCount(total: Int, theList: List[T]): Int = {
      if (theList.isEmpty) {
        total
      } else {
        startCount(total + 1, theList.tail)
      }
    }
    startCount(0, ourList)
  }

  def getReverse[T](original: List[T]): List[T] = {
    def backwards[T](forward: List[T], reversed: List[T]): List[T] = forward match {
      case List() => reversed
      case current :: others => backwards(others, current +: reversed)
    }
    backwards(original, List())
  }

  def palindrome[T](listPassed: List[T]): Boolean = listPassed match {
    case List() => true
    case current :: List() => true
    case current :: others =>
      if (current != others.last) {
        false
      } else {
        palindrome(others.dropRight(1))
      }
  }

  def duplicate[_](mainList: List[_]): List[_] = {
    if (mainList.isEmpty) Nil
    else List.fill(2)(mainList.head) ::: duplicate(mainList.tail)
  }

  def split[_](i: Int, original: List[_]): List[_] = {
    def subLists[_](index: Int, source: List[_], target: List[_]): List[_] = index match {
      case 0 => List(target, source)
      case _ => subLists(index - 1, source.tail, target :+ source.head)
    }
    subLists(i, original, List())
  } 
}