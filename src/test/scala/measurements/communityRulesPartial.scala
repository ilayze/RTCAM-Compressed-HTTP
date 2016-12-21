package measurements

import algorithm.{algorithmResult, rtcamCompressedHttp}
import com.jcraft.jzlib.Converter
import org.scalatest.FunSuite
import src.main.com.jce.{gzipPacket, tcamSimulator}

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 9/6/2016.
  */
class communityRulesPartial extends FunSuite {
      val allAbc = "abcdefghijklmnopqrstuvwxyz"
      test("partial snort community rules"){
        var results = ListBuffer[algorithmResult]()
        for(i<-5 until(50) by 5){
          val tcamSimulator = new tcamSimulator(width = i)
          tcamSimulator.printTcam = false
          tcamSimulator.initializeWithParser("/rules/community-rules-partial.txt")

          val gzipAscii = Converter.ToGzipAscii(allAbc+"Subject|3A 20|HawkEye Keylogger|20 7C 20|"+allAbc)
          val gzipPacket = new gzipPacket(gzipAscii)

          val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
          val algorithmResult = rtcamCompressedHttp.execute()
          results.append(algorithmResult)

          assert(algorithmResult.matchList.length.equals(1))
        }

        for (res<-results){
          println("Width "+res.measurements.tcamWidth)
          println("Shift average: %s".format(res.measurements.shiftSum.toFloat / res.measurements.lookupCounter))

        }
      }

      test("Naive vs Compressed"){
        var resultsCompressed = ListBuffer[algorithmResult]()
        var resultsNaive = ListBuffer[algorithmResult]()
        for(i<-5 until(50) by 5){
          val tcamSimulator = new tcamSimulator(width = i)
          tcamSimulator.printTcam = false
          tcamSimulator.initialize(allAbc)

          val gzipAsciiCompressed = Converter.ToGzipAscii(allAbc+allAbc+allAbc)
          val gzipPacketCompressed = new gzipPacket(gzipAsciiCompressed)

          var gzipAsciiNaive = Converter.ToGzipAscii(allAbc)
          gzipAsciiNaive =gzipAsciiNaive+gzipAsciiNaive+gzipAsciiNaive
          val gzipPacketNaive = new gzipPacket(gzipAsciiNaive)

          val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacketCompressed, tcam = tcamSimulator)
          val algorithmResult = rtcamCompressedHttp.execute()
          resultsCompressed.append(algorithmResult)

          val rtcamCompressedHttpNaive = new rtcamCompressedHttp(packet = gzipPacketCompressed, tcam = tcamSimulator)
          val algorithmResultNaive = rtcamCompressedHttpNaive.execute()

          resultsNaive.append(algorithmResultNaive)
         // assert(algorithmResult.matchList.length.equals(3))
        }

        printResults(resultsCompressed, resultsNaive)
      }

  def printResults(resultsCompressed: ListBuffer[algorithmResult], resultsNaive: ListBuffer[algorithmResult]): Unit = {
    for (i <- 0 until (resultsCompressed.length)) {
      println("$$$$$$$$$$     Width " + resultsCompressed(i).measurements.tcamWidth + "     $$$$$$$$$$")
      val skipAverageCompressed = (resultsCompressed(i).measurements.shiftSum.toFloat + resultsCompressed(i).measurements.numberOfCompressed) / resultsCompressed(i).measurements.lookupCounter
      val skipAverageNaive = (resultsNaive(i).measurements.shiftSum.toFloat + resultsNaive(i).measurements.numberOfCompressed) / resultsNaive(i).measurements.lookupCounter
      println("Skip average compressed: %s, Naive: %s".format(skipAverageCompressed, skipAverageNaive))
      println("Memory access compressed: %s,Naive: %s".format(resultsCompressed(i).measurements.memoryAccessCounter, resultsNaive(i).measurements.memoryAccessCounter))
      println("TCAM lookup compressed: %s, Naive: %s".format(resultsCompressed(i).measurements.lookupCounter, resultsNaive(i).measurements.lookupCounter))
      println("Compression ratio: %s".format(resultsCompressed(i).measurements.compressionRatio))
      println("")
    }
  }
}
