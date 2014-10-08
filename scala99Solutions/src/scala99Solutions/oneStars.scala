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
}