package com.practice.scala.Feature

import java.io.{File => JFile}

import scala.reflect.io.File

object Test1 extends App{

 val d = new JFile(".")

 d.listFiles().map(x => x.toString).filter(_.endsWith(".txt")).foreach(println)

  val pre = List(List(1,2),List(3,4),List(5,6))

// println(pre.map(_.map(_+1)))

 // println(pre.flatMap(_.map(_+1)))


  val files = List("abx.txt","sys.txt","que.scala")

 // for(file <- files if (file.endsWith(".scala"))) println(file)

  var testMap = Map("a" -> 1,"b" -> 2, "c" -> 3 )

//  println(testMap.toList.map(_._2))



//  scala.io.

}
