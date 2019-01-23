import org.apache.spark.sql.SparkSession
import org.apache.spark.{SparkConf, SparkContext}

object Test extends App{

  val conf = new SparkConf().setAppName("MyApplication1").setMaster("local[*]")

  val sc = new SparkContext(conf)

   val spark = SparkSession.builder().appName("").getOrCreate()

  import spark.implicits._

  val personList = List(Person("Sam",23,4000),Person("Leela",24,5000))

  val personRdd = sc.parallelize(personList)

  val personDf = personRdd.toDF("this is name","thisisage","thissal")

  personDf.show

 // val spark = SparkSession.builder().appName("").getOrCreate()

  val input = sc.textFile("input.txt")

  val words = input.flatMap(_.split(" ")).map(x => (x,1)).reduceByKey(_+_)

  //words.foreach(println)

}

case class Person(name:String,age:Int,salary:Double)
