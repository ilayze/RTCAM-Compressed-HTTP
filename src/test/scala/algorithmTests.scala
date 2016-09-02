import algorithm.rtcamCompressedHttp
import com.jcraft.jzlib.Converter
import org.scalatest.FunSuite
import src.main.com.jce.{gzipPacket, tcamSimulator}

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/15/2016.
  */
class algorithmTests extends FunSuite {
  test("Pattern and tcam width are equal") {
    val matchedList: ListBuffer[Int] = runAlgorithmFlow() //use default parameters

    assert(matchedList.length.equals(2))
  }

  test("Pattern less than tcam width"){
    val matchedList: ListBuffer[Int] = runAlgorithmFlow(tcamPattern = "he")

    assert(matchedList.length.equals(2))
  }

  test("Pattern greater than tcam width"){
    val matchedList: ListBuffer[Int] = runAlgorithmFlow(tcamPattern = "hello!")

    assert(matchedList.length.equals(1))
  }

  test("Naive algorithm - no pointers"){
    val matchedList: ListBuffer[Int] = runAlgorithmFlow(tcamPattern = "abcd",tcamPackage = "treotp,abcd,glm")

    assert(matchedList.length.equals(1))
    assert(matchedList(0).equals(11))
  }

  test("Naive algorithm - pattern greater than tcam width"){
    val matchedList: ListBuffer[Int] = runAlgorithmFlow(tcamPattern = "abcdef",tcamPackage = "abcdef")

    assert(matchedList.length.equals(1))
    assert(matchedList(0).equals(6))

  }

  test("Naive algorithm - pattern greater than tcam width and divisble with width"){
    val matchedList: ListBuffer[Int] = runAlgorithmFlow(tcamPattern = "abcdef",tcamPackage = "abcdef",tcamWidth = 3)

    assert(matchedList.length.equals(1))
    assert(matchedList(0).equals(6))

  }

  test("Left pointer match"){
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("dabc")

    val gzipAscii = "C97;C98;C99;C100;L4;D4;" //abcd[4,4]
    val gzipPacket = new gzipPacket(gzipAscii)

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val matchedList = rtcamCompressedHttp.execute()

    assert(matchedList.length.equals(1))
    assert(matchedList(0).equals(7))

  }

  test("right pointer match"){
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("abc")
    val gzipAscii = "C100;C100;C100;C100;C100;C97;C98;L7;D7;C99;C99;C99" //dddddab[7,7]ccc
    val gzipPacket = new gzipPacket(gzipAscii)

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val matchedList = rtcamCompressedHttp.execute()

    assert(matchedList.length.equals(1))
    assert(matchedList(0).equals(15))
  }



  def runAlgorithmFlow(tcamWidth:Int =5,tcamPattern:String="hello",tcamPackage:String="hello, hello! "): ListBuffer[Int] = {
    val tcamSimulator = new tcamSimulator(width = tcamWidth)
    tcamSimulator.initialize(tcamPattern)

    val gzipAscii = Converter.ToGzipAscii(tcamPackage)
    val gzipPacket = new gzipPacket(gzipAscii) //L4;D7; = 7 steps backward take 4 characters

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val matchedList = rtcamCompressedHttp.execute()
    matchedList
  }
}

