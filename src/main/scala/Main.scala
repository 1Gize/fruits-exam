import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource

object Main extends App{
  val srcHarv = io.Source.fromFile("csv/harvest.csv")
  val srcPrice = io.Source.fromFile("csv/prices.csv")
  val harvList = new ListBuffer[(String,Date,String,Double)]
  val priceList = new ListBuffer[(String,Date,Double)]
  val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
  for(line <- srcHarv.getLines().drop(1)){
    val cols = line.split(",").map(_.trim)
    harvList += Tuple4(cols(0),format.parse(cols(1)),cols(2),cols(3).toDouble)
  }
  srcHarv.close()
  for(line <- srcPrice.getLines().drop(1)){
    val cols = line.split(",").map(_.trim)
    priceList += Tuple3(cols(0),format.parse(cols(1)),cols(2).toDouble)
  }
  srcPrice.close()
  def yearlyBestHarvesters() ={
    println(bestHarveserInAMonth(format.parse("2020-02-1"),format.parse("2019-12-31")))
    println(bestHarveserInAMonth(format.parse("2020-03-1"),format.parse("2020-01-31")))
    println(bestHarveserInAMonth(format.parse("2020-04-1"),format.parse("2020-02-29")))
    println(bestHarveserInAMonth(format.parse("2020-05-1"),format.parse("2020-03-31")))
    println(bestHarveserInAMonth(format.parse("2020-06-1"),format.parse("2020-04-30")))
    println(bestHarveserInAMonth(format.parse("2020-07-1"),format.parse("2020-05-31")))
    println(bestHarveserInAMonth(format.parse("2020-08-1"),format.parse("2020-06-30")))
    println(bestHarveserInAMonth(format.parse("2020-09-1"),format.parse("2020-07-31")))
    println(bestHarveserInAMonth(format.parse("2020-10-1"),format.parse("2020-08-31")))
    println(bestHarveserInAMonth(format.parse("2020-11-1"),format.parse("2020-09-30")))
    println(bestHarveserInAMonth(format.parse("2020-12-1"),format.parse("2020-10-31")))
    println(bestHarveserInAMonth(format.parse("2021-01-1"),format.parse("2020-11-30")))
  }

  def bestHarveserInAMonth(before: Date, after: Date) ={
    findBiggest(aggregateAmounts(listInMonths(before,after)))
  }
  def harvestersAndFriuts(before: Date,after: Date): Map[(String,String),Double] ={
   val x =  listInMonths(before,after).groupMap(f=> (f._1,f._3))(f => f._4)
   val listOfFruitsAndAmounts = x.map(f => (f._1,f._2.sum))
    listOfFruitsAndAmounts
  }
  def bestHarvesterInFruit(before: Date, after: Date) ={
    val x = harvestersAndFriuts(before,after)
    val best = x.max
    best
  }
  def listInMonths(before: Date, after: Date)={
      harvList.filter(p => p._2.before(before) && p._2.after(after))
  }
  def aggregateAmounts(ls: ListBuffer[(String,Date,String,Double)]) ={
    val x = ls.groupMap(f => f._1)(f => f._4)
    x.map(f =>(f._1, f._2.sum))
  }
  def findBiggest(ls: Map[String,Double]) ={
    val biggest = ls.max
    biggest
  }
  yearlyBestHarvesters()
  println(bestHarvesterInFruit(format.parse("2020-02-1"),format.parse("2019-12-31")))

}
