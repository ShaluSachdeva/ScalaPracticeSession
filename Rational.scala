package com.practice.scala.Feature

class Rational(n: Int, d: Int) {

  private val g = gcd(n.abs,d.abs)
  val numer = n / g
  val denom = d / g
  require(d != 0)
  override def toString = numer +"/"+ denom

  def this(n:Int) = this (n,1)

  def + (that: Rational): Rational =
    new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def + (i:Int):Rational = new Rational(i*denom + numer, denom)

  def * (that:Rational):Rational =
    new Rational(numer * that.numer,denom * that.denom)

  def * (i:Int): Rational =
    new Rational(i * numer, denom)

  def - (that: Rational): Rational =
    new Rational(numer * that.denom - that.numer * denom, denom * that.denom)

  def - (i:Int):Rational = new Rational(i*denom - numer, denom)

  def / (that:Rational):Rational =
    new Rational(numer * that.denom, denom * that.numer)

  def / (i: Int): Rational =
    new Rational(numer, denom * i)

  def gcd(n:Int,d:Int):Int = {
    if(d == 0) n else gcd(d, n%d)
  }
}

object TestRational extends App {
  val x = new Rational(4, 6)
  val y = new Rational(66,42)

  println(x * y)

}
