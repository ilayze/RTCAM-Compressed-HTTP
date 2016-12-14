import algorithm.{algorithmResult, rtcamCompressedHttp}
import com.jcraft.jzlib.Converter
import org.scalatest.FunSuite
import src.main.com.jce.{gzipPacket, tcamSimulator}

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/15/2016.
  */
class algorithmTests extends FunSuite {

  test("Pattern and tcam width are equal") {
    val algorithmResult = runAlgorithmFlow() //use default parameters

    assert(algorithmResult.matchList.length.equals(2))
  }

  test("Pattern less than tcam width") {
    val algorithmResult = runAlgorithmFlow(tcamPattern = "he")

    assert(algorithmResult.matchList.length.equals(2))
  }

  test("Pattern greater than tcam width") {
    val algorithmResult = runAlgorithmFlow(tcamPattern = "hello!")

    assert(algorithmResult.matchList.length.equals(1))
  }

  test("Naive algorithm - no pointers") {
    val algorithmResult = runAlgorithmFlow(tcamPattern = "abcd", tcamPackage = "treotp,abcd,glm")

    assert(algorithmResult.matchList.length.equals(1))
    assert(algorithmResult.matchList(0).equals(11))
  }

  test("Naive algorithm - pattern greater than tcam width") {
    val algorithmResult = runAlgorithmFlow(tcamPattern = "abcdef", tcamPackage = "abcdef")

    assert(algorithmResult.matchList.length.equals(1))
    assert(algorithmResult.matchList(0).equals(6))

  }

  test("Naive algorithm - pattern greater than width and no match in the end of the pattern") {
    val algorithmResult = runAlgorithmFlow(tcamPattern = "abcdefg", tcamPackage = "abcabb", tcamWidth = 3)

    assert(algorithmResult.matchList.length.equals(0))
  }

  test("Naive algorithm - pattern greater than tcam width and divisble with width") {
    val algorithmResult = runAlgorithmFlow(tcamPattern = "abcdef", tcamPackage = "abcdef", tcamWidth = 3)

    assert(algorithmResult.matchList.length.equals(1))
    assert(algorithmResult.matchList(0).equals(6))

  }

  test("Left pointer match") {
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("dabc")

    val gzipAscii = "C97;C98;C99;C100;L4;D4;" //abcd[4,4]
    val gzipPacket = new gzipPacket(gzipAscii)

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val algorithmResult = rtcamCompressedHttp.execute()

    assert(algorithmResult.matchList.length.equals(1))
    assert(algorithmResult.matchList(0).equals(7))

  }

  test("right pointer match") {
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("abc")
    val gzipAscii = "C100;C100;C100;C100;C100;C97;C98;L7;D7;C99;C99;C99" //dddddab[7,7]ccc
    val gzipPacket = new gzipPacket(gzipAscii)

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val algorithmResult = rtcamCompressedHttp.execute()

    assert(algorithmResult.matchList.length.equals(1))
    assert(algorithmResult.matchList(0).equals(15))
  }

  test("Shift 0 but no match") {
    val algorithmResult = runAlgorithmFlow(tcamPattern = "abcdef", tcamPackage = "abcabc", tcamWidth = 3)

    assert(algorithmResult.matchList.length.equals(0))

  }

  test("More example Shift 0 but no match") {
    val algorithmResult = runAlgorithmFlow(tcamPattern = "abcdefghi", tcamPackage = "abcabcabc", tcamWidth = 3)

    assert(algorithmResult.matchList.length.equals(0))

  }

  test("Internal pointer match") {
    val tcamSimulator = new tcamSimulator(width = 3)
    tcamSimulator.initialize("abc")
    val gzipAscii = "C100;C100;C100;C97;C98;C99;C100;C100;C100;C100;C100;C100;L10;D12;C99;C99;C99" //dddabcdddddd[L10,D12]ccc
    val gzipPacket = new gzipPacket(gzipAscii)

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val algorithmResult = rtcamCompressedHttp.execute()

    assert(algorithmResult.matchList.length.equals(2))
    assert(algorithmResult.matchList(0).equals(6))
    assert(algorithmResult.matchList(1).equals(18))
  }

  test("Internal pointer match - long pattern") {
    val tcamSimulator = new tcamSimulator(width = 3)
    tcamSimulator.initialize("abcabcabcabc")
    val gzipAscii = "C97;C98;C99;L21;D3;" //abc[21,3]
    val gzipPacket = new gzipPacket(gzipAscii)

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val algorithmResult = rtcamCompressedHttp.execute()
    for(matchIns <- algorithmResult.matchList){
      println("match: "+matchIns.toString)
    }

    assert(algorithmResult.matchList.length.equals(5))
    assert(algorithmResult.matchList(0).equals(12))
    assert(algorithmResult.matchList(1).equals(15))
    assert(algorithmResult.matchList(2).equals(18))
    assert(algorithmResult.matchList(3).equals(21))
    assert(algorithmResult.matchList(4).equals(24))
  }

  test("Internal pointer to pointer") {
    val tcamSimulator = new tcamSimulator(width = 3)
    tcamSimulator.initialize("abcabcabcabc")
    val gzipAscii = "C97;C98;C99;L21;D3;L21;D21;" //abc[21,3][21,3]
    val gzipPacket = new gzipPacket(gzipAscii)

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val algorithmResult = rtcamCompressedHttp.execute()
    for(matchIns <- algorithmResult.matchList){
      println("match: "+matchIns.toString)
    }

    /*assert(algorithmResult.matchList.length.equals(5))
    assert(algorithmResult.matchList(0).equals(12))
    assert(algorithmResult.matchList(1).equals(15))
    assert(algorithmResult.matchList(2).equals(18))
    assert(algorithmResult.matchList(3).equals(21))
    assert(algorithmResult.matchList(4).equals(24))*/
  }



  test("No match at all - shift average should be equal to width"){
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("abcde")
    val gzipAscii = "C112;C112;C112;L2;D3;C101;C101;C101;L2;D2;C114;C114;C114;C114;C114;C116;C116;C116;L2;D2;" //pppppeeeeerrrrrttttt
    val gzipPacket = new gzipPacket(gzipAscii)

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val algorithmResult = rtcamCompressedHttp.execute()

    assert(algorithmResult.measurements.tcamWidth.equals(algorithmResult.measurements.shiftSum/algorithmResult.measurements.lookupCounter))

  }

  test("same substring in signature"){
    val algorithmResult = runAlgorithmFlow(tcamPattern = "abcabc", tcamPackage = "abcabc", tcamWidth = 3)
    assert(algorithmResult.matchList.length.equals(1))
  }

  test("pattern less than width in the end of the packet"){
    val abcd = "abcd"
    val algorithmResult = runAlgorithmFlow(tcamPattern = abcd, tcamPackage = abcd, tcamWidth = 5)
    assert(algorithmResult.matchList.length.equals(1))
  }


  def runAlgorithmFlow(tcamWidth: Int = 5, tcamPattern: String = "hello", tcamPackage: String = "hello, hello! "): algorithmResult = {
    val tcamSimulator = new tcamSimulator(width = tcamWidth)
    tcamSimulator.initialize(tcamPattern)

    val gzipAscii = Converter.ToGzipAscii(tcamPackage)
    val gzipPacket = new gzipPacket(gzipAscii) //L4;D7; = 7 steps backward take 4 characters

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val algorithmResult = rtcamCompressedHttp.execute()
    algorithmResult
  }
}

