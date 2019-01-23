package com.practice.scala.Feature

import scala.collection.mutable

object StringExercise extends App {

  def countChars(input:String):mutable.Map[Char,Int] = {
    input.foldLeft(mutable.Map[Char,Int]()){
      (r,c) => val count = input.count(_==c)
        r.put(c,count)
        r
    }
  }

  def getDuplicateFromString(input:String):String ={
    (for(c <- input if (input.count(_==c) > 1))
      yield c).foldLeft(""){
      (acc,r) => if (acc.contains(r)) acc else acc.concat("" + r)
    }
  }

  def getUniqueFromString(input:String):String = {
    input.foldLeft(""){
      (acc,r) => if(acc.contains(r)) acc else acc.concat("" + r)
    }
  }


  def ifAnagram(input1:String,input2:String):Boolean = {
    input1.toLowerCase.sortWith(_<_).equals(input2.toLowerCase.sortWith(_<_))
  }

  def getNonRepeatedFromString(input:String):Char = {
    (for(x <- input if (input.count(_ == x) == 1))
      yield x).charAt(0)
  }

  def reverseAString(input:String):String = {
    ( for (i <- input.length-1 to 0 by -1) yield input(i)).mkString
  }

  def getCountOfDuplicate(input:String) = {
    var count = 0
    var dupCount = mutable.Map[Char,Int]()
    for(i <- input){
      count = input.count(_==i)
    if (count > 1)
    dupCount.put(i,count)
    }
    dupCount
  }

  def getAllPermutationsOfString(input:String) = {

  }

  def sum(a:Int,b:Int) = a+ b
  val a = sum _


  def echo(args:String*) = for(arg <- args)println(arg)

  def reverseWordsInSentence(input:String) = {
    input.split(" ").foldLeft("") {
      (r, c) =>
        c + " " + r
    }
  }

  def removeDuplicate(input:String) = {
    input.foldLeft("")(
      (r,c) => if (r.contains(c)) r else r + c
    )
  }

  def getIndexOfSub(input:String,sub:String) = {
    input.indexOf(sub)
  }

def highestOccurance(input:String) = {
  input.foldLeft(mutable.Map[Char, Int]()) {
    (r, c) =>
      val count = input.count(_ == c)
      r.put(c, count)
      mutable.Map(r.maxBy(_._2))
  }
}

  def highestOccuranceOfWord(input:String) = {
    input.split(",").map{
      x => (x,x.length)
    }.maxBy(_._2)
  }

  def removeChar(input:String,ch:Char) = {
    input.foldLeft(""){
      (r,c) => if (c==ch) r else r + c
    }
  }

  def getLongestPalindrom(input:String) = {
    for(x <- 0 to input.length-1 )
      for(y <- 0 to input.length-1 if x > y && input.substring(x,y) == input.substring(x,y).reverse)
          yield input.substring(x,y)
  }


  println(countChars("hackecnhhtrraaar"))

}
