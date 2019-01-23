package com.practice.scala.Feature

import scala.annotation.tailrec
import scala.util.control.Breaks._

object SearchAlgo extends App{

  val arr = (1 to 1000000).toArray

  //arr.withFilter()

  def linearSearchElementInArr (element:Int,arr:Array[Int]):Int = {
    var position = 0
    breakable {
      for (x <- 0 until arr.length if (arr(x) == element))
          position = x
          break
      }
    position
  }

  def binarySearchElementInArr (value:Int,arr:Array[Int],start:Int,end:Int):Int = {
    val mid = (start+end)/2
    if(start > end) {
      println("Not Found")
      return -99999999
    }
    arr(mid) match {
      case x if (x == value) =>  mid
      case x if (value < x) => binarySearchElementInArr (value,arr,start,mid-1)
      case x if (value > x) =>  binarySearchElementInArr (value,arr,mid+1,arr.length-1)
    }
  }



@tailrec
  def lastElement(l1:List[Any]):Any = l1 match {
    case x::Nil => x
    case x:: xs => lastElement(xs)
    case _ => throw new NoSuchElementException
  }
  @tailrec
  def secondLastElement(l1:List[Any]):Any = l1 match {
    case x::y::Nil => x
    case x:: xs => secondLastElement(xs)
    case _ => throw new NoSuchElementException
  }

  def findKthElement(position:Int,l1:List[Any]):Any = (position,l1) match{
    case (0,x::xs) => x
    case (position,head::tail) => findKthElement(position - 1,tail)
    case _ => throw new NoSuchElementException

  }

  def findlength(l1:List[Int]):Int = {

    l1.foldLeft(0) { (x, y) => x + 1
    }
  }

  def recursiveLength(l1:List[Any]):Any ={
      def tailR(acc:Int,l1:List[Any]):Any = l1 match{
        case Nil => acc
        case (x::tail) =>tailR (acc+1,tail)
      }
  tailR(0,l1)
  }

  println(recursiveLength(List(1,2,3,4,5,9)))

  def lengthRecursive[A](ls: List[A]): Int = ls match {
    case Nil       => 0
    case _ :: tail => 1 + lengthRecursive(tail)
  }

  def reverseList(l1:List[Any]):List[Any]={
    def tailReverse(l1:List[Any],currList:List[Any]):List[Any] = l1 match{
      case Nil => currList
      case x:: xs => tailReverse(xs,x::currList)
    }
    tailReverse(l1,List[Any]())
  }
  def reverseFuncList(l1:List[Any]):List[Any]={
    l1.foldLeft(List[Any]()){(x,y) => y::x}
  }

  def isPalindrome(l1:List[Any]):Boolean = {
    l1 == l1.reverse
  }

  def recIsPalindrome(l1:List[Any]):Boolean = l1 match{
    case x::Nil => true
    case (x::xs) if (x == xs.last) => recIsPalindrome(xs.init)
    case (x::xs) => false

  }

/*  def flatTheNestedList(l1:List[Any]):List[Any] = {

      def tailFlatten(result:List[Any],running:List[Any]):List[Any] = running match {
        case(x:List[_])::xs => x:::result
        case (x::xs) =>

  }*/

  def compressList(l1:List[Any]):List[Any] = {

    def tailCompress(result: List[Any], running: List[Any]): List[Any] = running match{
      case Nil => result.reverse
      case (x :: tail) if (x == tail.head) => tailCompress(x :: result, tail.dropWhile(_ == x))
      case (x :: tail) => tailCompress(x::result,tail)
    }
    tailCompress(List[Any](), l1)
  }

  def compressListFunc(l1:List[Any]):List[Any] = {
    l1.foldRight(List[Any]()){
      (x,y) => if ( y.isEmpty || y.head != x) x::y
      else y
    }
  }

  def duplicateListElements(l1:List[Any]):List[Any] = {

    def tailDupList( result:List[Any],l1:List[Any]):List[Any] = l1 match{
      case Nil => result.reverse
      case x::xs => tailDupList(x::x::result,xs)
    }
    tailDupList(Nil,l1)

    //l1.foldLeft(List[Any]()){(x,y) => y::y::x}.reverse
  }

  def duplicateN(l1:List[Any],number:Int):List[Any] = {
    l1.flatMap(List.fill(number)(_))
  }

/*  def dropNthElement(l1:List[Any],position:Int):List[Any] = {
    l1

  }*/


  def hasSublist[T](list: List[T], sub: List[T]): Boolean = {
    def matchSub(ls: List[T], ss: List[T]): Option[Boolean] = (ls, ss) match {
      case (_, Nil) => Some(true)
      case (Nil, _) => None
      case (x :: xs, y :: ys) if x == y => matchSub(xs, ys)
      case (_, _) => Some(false)
    }

    matchSub(list, sub) match {
      case None => false
      case Some(false) => hasSublist(list.tail, sub)
      case _ => true
    }
  }

  def evaluate[T](t: T)(implicit value: Numeric[T]): Unit = {
    println(value)
  }

  val pattern1 = "([0-9]{3})-([0-9]{3})-([0-9]{4})".r

  val colors1 = Map("Red" -> "#FF0000")
  val colors2 = Map("Blue" -> "#0000FF")
  val colors3 = Map("Green" -> "#00FF00")

  var Colors =   colors1 +("" -> "")

  Colors = colors2 ++colors2++colors3

  import scala.util.Try

  def doDiv(value1: Int, value2: Int) : Try[Int] = Try(value1 / value2)

 // print(doDiv(5, 0))

  def GetColor(number: Int): String = number match {
    case 1 => "Red"
    case 2 => "Orange"
    case 3 => "Yellow"
    case 4 => "Green"
    case 5 => "Blue"
    case 6 => "Purple"
  }

  val pattern = "[0-9]".r
  val search = "Hello2You"
 // val result = pattern.findFirstIn.search
//  GetColor(result.get.toInt)

//pattern "\\d".r

  var numList = List(1,2,3,4)
  val arr2 = new Array [Int](10)
  val arr3 = Array(20,22)

  numList.copyToArray(arr2)

  val it = numList.iterator

  it.next

  arr2.foreach(println)
  arr3.foreach(println)

  println(numList.mkString)
  var strList = List("One", "Two", "Three", "Four")

  var strArray = strList.toArray
  var numArray = numList.toArray
 // strList.toMap(numList)
//
  implicit class ProcessString (s: String) {
    def initialCap = {
      var Value = "";
      s.split(" ").foreach{ _.toUpperCase() + " " }
      Value
    }
  }

 /* class Numbers(var favoriteNum:Int)
  def myNum = new Numbers(12)
 // myNum.favoriteNum(20)
//  println(myNum.favoriteNum)



  var p = (1,2,3,4)
*/


/* // val pattern = "[a-zA-Z]{4,5}".r

//  val search = "One Two Three Four"
  val matches = pattern findAllIn search
  matches.foreach { e =>
    println(e)
  }*/

 // println(hasSublist(List(1,2,3,244,444,88),List(2,3,5)))
 // println(binarySearchElementInArr(99999,arr,0,arr.length-1))
  //println(linearSearchElementInArr(99999,arr))
}
