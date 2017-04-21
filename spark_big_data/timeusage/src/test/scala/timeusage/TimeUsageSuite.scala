package timeusage

import java.util

import org.apache.spark.rdd.RDD
import org.apache.spark.sql.functions._
import org.apache.spark.sql.{Column, DataFrame, Row, SparkSession}
import org.junit.runner.RunWith
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll with Matchers {
  val spark: SparkSession =
    SparkSession
    .builder()
    .appName("Time Usage")
    .config("spark.master", "local")
    .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  val expectedPrimaryColumns: List[Column] = List(col("t01"), col("t03"))
  val expectedWorkColumns: List[Column] = List(col("t05"), col("t1805"))
  val expectedOtherColumns = List(col("t02"), col("t04"))

  test("columns names should be converted into columns according to their prefix") {
    val columnHeaders = List("t01", "t02", "t03", "t04", "t05", "t1805", "t9999")
    val columns = TimeUsage.classifiedColumns(columnHeaders)

    columns match {
      case (primary, work, leisure) =>
        primary should contain theSameElementsAs expectedPrimaryColumns
        work should contain theSameElementsAs expectedWorkColumns
        leisure should contain theSameElementsAs expectedOtherColumns
    }
  }

  test("summary of data is generated") {
    val csvData =
      """tucaseid,t01,t02,t03,t04,t05,t1805,teage,tesex,telfs
        |"id1",1,2,3,4,5,6,18,1,2
        |"id2",7,8,9,10,11,12,56,0,3
        |"id3",13,14,15,16,17,18,40,1,5"""
      .stripMargin.split("\n")

    val csvRdd: RDD[String] = spark.sparkContext.parallelize(csvData)
    val (_, df): (List[String], DataFrame) = TimeUsage.readCsv(csvRdd)
    val summary: DataFrame = TimeUsage
                             .timeUsageSummary(expectedPrimaryColumns, expectedWorkColumns, expectedOtherColumns, df)

    // select(workingStatusProjection, sexProjection, ageProjection, primaryNeedsProjection, workProjection, otherProjection)
    summary.count() shouldBe 2
    val rows: util.List[Row] = summary.collectAsList()
    val firstRow = rows.get(0)
    val secondRow = rows.get(1)

    implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(0.0001)

    firstRow.getString(0) shouldBe "working"
    secondRow.getString(0) shouldBe "not working"

    firstRow.getString(1) shouldBe "male"
    secondRow.getString(1) shouldBe "female"

    firstRow.getString(2) shouldBe "young"
    secondRow.getString(2) shouldBe "elder"


    assert(firstRow.getDouble(3) === (1.0 + 3.0) / 60)
    assert(secondRow.getDouble(3) === (7.0 + 9.0) / 60)

    assert(firstRow.getDouble(4) === (5.0 + 6.0) / 60)
    assert(secondRow.getDouble(4) === (11.0 + 12.0) / 60)

    assert(firstRow.getDouble(5) === (2.0 + 4.0) / 60)
    assert(secondRow.getDouble(5) === (8.0 + 10.0) / 60)
  }
}
