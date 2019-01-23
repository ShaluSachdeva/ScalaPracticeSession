package com.practice.scala.Feature

import scala.util.control.Breaks._

object SortAlgo extends App{

  def selectionSort(l1:List[Int]):List[Int] = {

      def tailSelectionSort(l1:List[Int],result:List[Int]):List[Int] = l1 match {
        case Nil => result.reverse
        case x :: xs => tailSelectionSort(l1.filter(_ != getMinOfList(l1)), getMinOfList(l1) :: result)
      }
    tailSelectionSort(l1,Nil)
  }

  def findMin(x:Int,y:Int):Int = if (x < y) x else y

  def getMinOfList(l1:List[Int]):Int = {
    l1.reduceLeft(findMin)
  }

  def sumOfList(l1:List[Int]):Int = {
    l1.foldLeft(0){
      (x,y) => x + y
    }
  }

  def productOfList(l1:List[Int]):Int = {
    l1.foldLeft(1)(_ * _)
  }

  def countOfList(l1:List[Any]):Int = {
    l1.foldLeft(0){(x,y) => x + 1}
  }

  def averageOfList(l1:List[Int]):Double = {
    l1.foldLeft(0)(_+_)/l1.foldLeft(0){(r,c) => r +1}
    }

  def lastOfList(l1:List[Any]):Any = l1 match {
    case x:: Nil => x
    case x:: xs => lastOfList(xs)
    case Nil => throw new NullPointerException
  }

  def lastList(l1:List[Any]):Any = {
    l1.foldLeft[Any](1){
      (r,c) => c
    }
  }

  def penultimateList(l1:List[Any]):Any = {
    l1.foldLeft(l1.head,l1.tail.head){
      (r,c) => (r._2,c)
    }._1
  }

  def ifContains(l1:List[Any],value:Any):Boolean = {
    l1.foldLeft(false){
      (x,y) => (x || y == value)
    }
  }

  def getFromList(l1:List[Any],position:Int):Any = {
    l1.foldLeft(l1.head, 0) {
      (x, y) => if (x._2 == position) x else (y, x._2 + 1)
    } match {
      case (value,index) if (index == position) => value
      case (_) => throw new IndexOutOfBoundsException
    }
  }

  def toString(l1:List[Any]):String = l1 match{
    case x::xs => xs.foldLeft("List(" + x){
      (x,y) => (x + "," + y )}+ ")"
    case Nil => "List()"

  }

  def reverseList(l1:List[Any]):List[Any] = {
    l1.foldLeft(List[Any]()){
      (x,y) => y::x
    }
  }

  def uniqueList(l1:List[Any]):List[Any] = {
    l1.foldLeft(List[Any]()){
      (x,y) => if (x.contains(y)) x else y::x
    }.reverse
  }

  def doubleTheList(l1:List[Any]):List[Any] = {
    l1.foldLeft(List[Any]()){
      (r,c) => c::c::r
    }.reverse
  }

  def insertionSort[A <% Ordered[A]](l1:List[A]):List[A] = {
    l1.foldLeft(List[A]()){
      (r,c) =>
        val (front,back) = r.span( _ < c)
        front:::c::back
    }
  }



  def longestWord(input:List[String]):String = {

    def tailLongest(result: String, input: List[String]): String = input match {
      case Nil => result
      case x :: xs if (x.length > result.length) => tailLongest(x, xs)
      case x :: xs => tailLongest(result, xs)
    }
    tailLongest("", input)
  }

  def sortArrayByLength(input: List[String]):List[String] = {

    def tailSort(result:List[String],input:List[String]):List[String] = input match {
      case Nil => result
      case x::xs => tailSort(findMax(input)::result,input.filter(_ != findMax(input)))
    }
    tailSort(Nil,input)
  }

  def findMax(input:List[String]):String = {
    input.reduceLeft(
      (x,y) => if (x.length > y.length) x else y
    )
  }



  def getSmallestMissingPositive(input:Array[Int]):Int = {
    val distinctList = input.distinct.sorted.toList
    val (negative,positive) = distinctList.span(_ < 0)
    def tailMin(result:Int,input1:List[Int]):Int =
      input1 match{
      case Nil => result
      case x::Nil => x+1
      case x::xs if (x+1 == xs.head) => tailMin(result,xs)
      case x::xs  => x+1
    }
    if (negative.isEmpty) tailMin(0,distinctList)
    else if (positive.contains(1)) tailMin(0,positive)
    else 1
  }

  def duplicateNTimes(n:Int,input:List[Any]):List[Any] = {

    input.foldLeft(List[Any]()){
      (r,c) => (for(x <- 1 to n) yield c).toList:::r
    }.reverse

  }

  def filterList(delim:Int,arr:List[Int]):List[Int] = {
    for(x <- arr if x < delim) yield x
  }

  def removeAtOdd(arr:List[Any]):List[Any] =  {
    (for(x <- 1 to arr.length-1 by 2) yield arr(x)).toList
  }

  def createList(n:Int):List[Int] = {
    (for (x <- 1 to n) yield (x)).toList
  }

  def sumOfOddElements(input:List[Int]):Int = {

    //(for(x <- input if x % 2 ==1 )yield (x)).sum

    input.foldLeft(0){
      (r,c) => if (c % 2 ==1 ) r+c else r+0
    }
  }

  def lengthOfList(arr:List[Int]):Int = {
    arr.foldLeft(0){
      (r,c) => r+1
    }
  }

  def updateListValues(arr:List[Int]):List[Int] = {
    for (x <- arr) yield Math.abs(x)
  }

  def mingleListItems(input1:List[Any],input2:List[Any]):List[Any] = {
    def tailMingle(result:List[Any],input1:List[Any],input2:List[Any]):List[Any] = (input1,input2) match {
      case (Nil,Nil) => result.reverse
      case (Nil,x::xs) => result.reverse:::x::xs
      case (x::xs,Nil) => result.reverse:::x::xs
      case (x::xs,y::ys) => tailMingle(x::y::result,xs,ys)
    }
    tailMingle(Nil,input1,input2)
  }

  def compressList(input:List[Any]):List[Any] = {

      def tailCompress(result:List[Any],input: List[Any]):List[Any] = input match {
        case Nil => result.reverse
        case x::xs => { val subsstr = xs.takeWhile(_==x);
          if (subsstr == 1) tailCompress(x::result,xs)
          else tailCompress(subsstr.size::x::result,xs.dropWhile(_ == x))}
        case x::xs => tailCompress(x::result,xs)
      }
    tailCompress(Nil,input)
  }

  def combinationList(l1:List[Int]) = {
    (for (x <- l1; y <- l1; z <- l1 if (x != y && x != z && y != z)) yield (x, y, z)).
      sortBy(x => (x._1,x._2,x._3))

  }







 // println(mingleListItems("",""))

}
