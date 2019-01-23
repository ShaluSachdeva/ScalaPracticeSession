package com.practice.scala.Feature

object PracticeScalaExercise extends App {

  // Create a List of N elements
  def createListOfNElements(length:Int):List[Int] = {
    (for (i <- 0 to length -1) yield i).toList
  }

  //Reverse elements of a List
  def reverseAList(input:List[Any]):List[Any] = {
    input.foldLeft(List[Any]()){
      (r,c) => c::r
    }
  }

  //Get Sum of elements at Odd places in a List
  def getSumOfOddElementsOfList(input:List[Int]):Int = {
    input.foldLeft(0){
      (r,c) => if (c % 2 == 1 ) r + c else r + 0
    }
  }

  //Duplicate elements of a List N times
  def replicateListNTimes(num:Int,input:List[Any]):List[Any] = {
    //for ( x <- input ; i <- 0 to num -1) yield x   <---- Solution using FOR Comprehension

    input.foldLeft(List[Any]()){
      (r,c) => (for (i <- 0 to num -1) yield c).toList ::: r
    }.reverse
  }

  //Filter elements less than delimiter from a List
  def filterList(deLimiter:Int,input:List[Int]):List[Int] = {
    for (i <- input if i < deLimiter) yield i
  }

  //Get elements at odd positions in a list
  def filterOddPositionInList(input:List[Any]):List[Any] = {
    //(for (i <- 1 to input.length -1 by 2) yield input(i)).toList   <--- Solution by FOR Comprehension

    input.foldLeft(0,List[Any]()){
      (r,c) => if (r._1 % 2 ==1) ( r._1+1,c::r._2) else (r._1+1, r._2)
    }._2.reverse
  }

  // Get length of a List
  def getLength(input:List[Any]):Int = {
    input.foldLeft(0){
      (r,c) => r + 1
    }
  }

  //Update elements of a List to absolute value
  def updateListElements(input:List[Int]):List[Int] = {
    input.foldLeft(List[Int]()){
      (r,c) => Math.abs(c)::r
    }.reverse
  }

  //Mingle the elements of two Strings
  def mingleTheStrings(input1:String,input2:String):String = {

    (for(i <- 0 to input1.length-1) yield (""+input1(i)+input2(i))).mkString
  }

  //Update the even indexed elements with next one in String
  def swapCharsAtEven(input:String):String = {
    input.foldLeft(0,""){
      (r,c) => if (r._1 % 2 == 0 ) (r._1+1,(c+1).toChar+r._2) else (r._1+1,c+r._2)
    }._2.reverse
  }

  //Compress the String: consecutive duplicate letters should be replaced by single one
  def stringCompression(input:String):String = {
/*    input.foldLeft("",input){
      (r,c) => if (r._2.takeWhile(_==c).length == 1) (c+r._1,r._2.dropWhile(_==c) )
      else (""+c+(r._2.takeWhile(_==c).length),r._2.dropWhile(_==c))
    }._1*/

    def loop(result:List[Any],input:List[Char]):List[Any] = input match {
      case Nil => result
      case (x::Nil) => x::result
      case x::xs if (x == xs.head) => loop(xs.takeWhile(_==x).length+1::x::result,xs.dropWhile(_==x))
      case x::xs => loop(x::result,xs)
    }

    loop(Nil,input.toList).reverse.mkString
  }

  //Check if the current year is Leap of Not
  def ifLeapYear(input:Int):Boolean = (input%4,input%100,input%400) match {
    case (_,_,0)  =>  true
    case (_,0,_) => false
    case (0,_,_) => true
    case _ => false
  }

  //Create List of Prime numbers if Start and End are given
  def createListOfPrime(start:Int,end:Int):List[Int] = {
    (for(i <- start to end ; j <- 1 to i/2 if i%j!=0) yield i).toList
  }

  //Sort the List of Lists by lengths of List elements
  def lSort(input:List[List[Any]]):List[List[Any]] = {
    input.sortBy(_.length)

   // input.map(x => (x,x.length)).sortBy(_._2).map(_._1)
  }

  //Duplicate the elements of the List
  def duplicateList(input:List[Any]):List[Any] = {
    input.foldLeft(List[Any]()){
      (r,c) => c::c::r
    }.reverse
  }

  //Duplicate elements of List N times
  def duplicateListNTimes(number:Int,input:List[Any]):List[Any] = {
    input.foldLeft(List[Any]()){
      (r,c) => (for (i <- 1 to number) yield c).toList:::r
    }.reverse
   // (for(i <- input; j<- 1 to number) yield i::Nil).flatten
  }

  //Drop Nth element of the list
  def dropNthElement(index:Int,input:List[Any]):List[Any] = {
    input.foldLeft(0,List[Any]()){
      (r,c) => if (r._1 != index) (r._1+1,c::r._2) else (r._1+1,r._2)
    }._2.reverse
  }

    //Split the List into two based on the given index
  def splitInTwo(position:Int,input:List[Any]):(List[Any],List[Any]) =  {
/*    input.foldLeft(0,List[Any](),List[Any]()){
      (r,c) => if (r._1 < position) (r._1+1,c::r._2,r._3)
        else(r._1+1,r._2,c::r._3)
    }*/
    def loop(pos:Int,pre:List[Any],input:List[Any]):(List[Any],List[Any]) = (pos,input) match{
      case(0,Nil) => (pre.reverse,Nil)
      case (0,x::xs) => (pre.reverse,x::xs)
      case(n,x::xs)=> loop(n-1,x::pre,xs)
    }
    loop(position,Nil,input)
  }

  //Get the Slice of the list based on the start and end position
  def slice(start:Int,end:Int,input:List[Any]) :List[Any] = {
    input.foldLeft(0,List[Any]()){
      (r,c) => if (r._1 >= start && r._1 < end) (r._1+1, c::r._2)
      else (r._1+1,r._2)
    }._2.reverse
  }

  //Rotate the List to n places
  def rotateNPlaces(place:Int,input:List[Any]):List[Any] = {
    def loop(place:Int,input:List[Any],result:List[Any]):List[Any] = (place,input) match {
      case(0,x::xs) => x::xs:::(result.reverse)
      case(place,x::xs) => loop(place-1,xs,x::result)
    }
    loop(place,input,List[Any]())
  }

    //Drop each Nth element of the List
  def dropEveryNthElement(index:Int,input:List[Any]):List[Any] = {
    input.foldLeft(1,List[Any]()){
      (r,c) => if (r._1 % index == 0) (r._1+1, r._2)
      else (r._1+1, c::r._2)
    }._2.reverse
  }

  //get the Kth element out of the List
  def getKthAndList(position:Int,input:List[Any]):(List[Any],Any) = {

    def loop(position:Int,input:List[Any],result:List[Any]):(List[Any],Any) = (position,input) match {
        case (0,Nil) => throw new NoSuchElementException
        case (0,x::xs) => (result.reverse:::xs,x)
        case (position,x::xs) =>  loop (position-1,xs,x::result)
    }
    loop(position,input,List[Any]())
  }

  //Insert the given element at a given position
  def insertAtPosition(element:Any,position:Int,input:List[Any]):List[Any] = {

    input.foldLeft(0,List[Any]()){
      (r,c) => if (r._1 != position) (r._1+1,c::r._2)
      else (r._1+1,c::element::r._2)
    }._2.reverse

  }

  println(insertAtPosition("new",2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))

}
