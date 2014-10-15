package scala99Solutions

object twoStars {
  def main(args: Array[String]) {

  }
  
  def flatten[_](mainList: List[_]): List[_] = {
    def combine[_](source: List[_], target: List[_]): List[_] = source match {
      case List() => target
      case current :: others => current match {
        case top: List[_] => combine(others, target ::: combine(top, List()))
        case _ => combine(others, target :+ current)
      }
    }
    combine(mainList, List())
  }
  
  def compress[T](mainList: List[T]): List[T] = {
    def reviewList[T](source: List[T], target: List[T]): List[T] = source match {
      case List() => target
      case current :: others => reviewList(others.dropWhile(_ == current), target :+ current)
    }
    reviewList(mainList, List())
  }
  
  def pack[T](mainList: List[T]): List[List[T]] = {
    def sublists[T](source: List[T], target: List[List[T]]): List[List[T]] = source match {
      case List() => target
      case current :: others => sublists(others.dropWhile(_ == current), target :+ source.takeWhile(_ == current))
    }
    sublists(mainList, List())
  }
  
  def encode[T](mainList: List[T]): List[(Int, _)] = {
    def countDups(source: List[List[_]], target: List[(Int, _)]): List[(Int, _)] = source match {
      case List() => target
      case current :: others => countDups(others, target :+ (current.length, current.head))
    }
    countDups(pack(mainList), List())
  }
  
  def encodeModified[T](mainList: List[T]): List[_] = {
    def makeElement[T](list: List[T]): Any = {
      if (list.length > 1) {
        (list.length, list.head)
      } else {
        list.head
      }
    }
    def countDups(source: List[List[_]], target: List[_]): List[_] = source match {
      case List() => target
      case current :: others => countDups(others, target :+ makeElement(current))
    }
    countDups(pack(mainList), List())
  }
  
  def decode[T](mainList: List[_]): List[_] = {
    def unwrap[T](source: List[_], target: List[_]): List[_] = source match {
      case List() => target
      case (total: Int, symbol) :: others => unwrap(others, target ::: List.fill(total)(symbol))
      case List(_) => List()
    }
    unwrap(mainList, List())
  }
  
  def encodeDirect[_](mainList: List[_]): List[_] = {
    def encode[_](source: List[_], target: List[(Int, _)]): List[(Int, _)] = source match {
      case List() => target
      case current :: others => encode(others.dropWhile(_ == current), target :+ (source.takeWhile(_ == current).length, current))
    }
    encode(mainList, List())
  }
  
  def duplicateN[_](total: Int, mainList: List[_]): List[_] = {
    if (mainList.isEmpty) Nil
    else (for (elem <- mainList) yield List.fill(total)(elem)).flatten
    //else List.fill(total)(mainList.head) ::: duplicateN(total, mainList.tail)
  }

  def drop[_](i: Int, mainList: List[_]): List[_] = {
    def splitEmUp[_](index: Int, source: List[_], target: List[_]): List[_] = (source, index) match {
      case (List(), _) => target
      case (_, 1) => splitEmUp(i, source.tail, target)
      case (_, _) => splitEmUp(index - 1, source.tail, target :+ source.head)
    }
    splitEmUp(i, mainList, List())
  }
}