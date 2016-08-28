import algorithm.rtcamCompressedHttp
import com.jcraft.jzlib.Converter
import org.scalatest.FunSuite
import src.main.com.jce.{gzipPacket, tcamSimulator}

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/15/2016.
  */
class algorithmTests extends FunSuite {
  test("Execute algorithm - pattern and tcam width are equal") {
    val matchedList: ListBuffer[Int] = runAlgorithmFlow() //use default parameters

    assert(matchedList.length.equals(2))
  }

  test("Execute algorithm - pattern less than tcam width"){
    val matchedList: ListBuffer[Int] = runAlgorithmFlow(tcamPattern = "he")

    assert(matchedList.length.equals(2))
  }

  test("Execute algorithm - pattern greater than tcam width"){
    val matchedList: ListBuffer[Int] = runAlgorithmFlow(tcamPattern = "hello!")

    assert(matchedList.length.equals(1))
  }



  def runAlgorithmFlow(tcamWidth:Int =5,tcamPattern:String="hello",tcamPackage:String="hello, hello! "): ListBuffer[Int] = {
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("hello")

    val gzipAscii = Converter.ToGzipAscii("hello, hello! ")
    val gzipPacket = new gzipPacket(gzipAscii) //"hello, hello! " L4;D7; = 7 steps backward take 4 characters

    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    val matchedList = rtcamCompressedHttp.execute()
    matchedList
  }
}

