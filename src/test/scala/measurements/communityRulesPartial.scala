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
      test("partial snort community rules"){
        var results = ListBuffer[algorithmResult]()
        for(i<-5 until(20)){
          val tcamSimulator = new tcamSimulator(width = i)
          tcamSimulator.printTcam = false
          tcamSimulator.initializeWithParser("/rules/community-rules-partial.txt")

          val gzipAscii = Converter.ToGzipAscii("Subject|3A 20|HawkEye Keylogger|20 7C 20|")
          val gzipPacket = new gzipPacket(gzipAscii)

          val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
          val algorithmResult = rtcamCompressedHttp.execute()
          results.append(algorithmResult)

          assert(algorithmResult.matchList.length.equals(1))
        }

        for (res<-results){
          println(res.measurements.tcamWidth)
          println("Shift average: %s".format(res.measurements.shiftSum / res.measurements.lookupCounter))

        }
      }

}
