package measurements

import algorithm.{algorithmResult, rtcamCompressedHttp}
import com.jce.OfflineCapturer
import com.jcraft.jzlib.Converter
import org.scalatest.FunSuite
import src.main.com.jce.{gzipPacket, tcamSimulator}

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer
import java.io._

/**
  * Created by izeidman on 9/6/2016.
  */
class communityRulesPartial extends FunSuite {
      val allAbc = "abcdefghijklmnopqrstuvwxyz"
      test("partial snort community rules"){
        var results = ListBuffer[algorithmResult]()
        for(i<-10 until(50) by 5){
          val tcamSimulator = new tcamSimulator(width = i)
          tcamSimulator.printTcam = false
          tcamSimulator.initializeWithParser("/rules/community-rules-partial.txt")

          val gzipAscii = Converter.ToGzipAscii(allAbc+"Subject: HawkEye Keylogger | "+allAbc)
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

      test("Real packets - from tcp dump"){
        var resultsCompressed = ListBuffer[algorithmResult]()
        var resultsNaive = ListBuffer[algorithmResult]()

        val oc = new OfflineCapturer()
        val payloads = oc.Capture("resources/outside.tcpdump",100).asScala
        var counter = 1
        for(packet <- payloads){

          println("Counter "+counter+":"+packet)
          counter+=1

          val tcamSimulator = new tcamSimulator(width = 10)
          tcamSimulator.printTcam = false
          tcamSimulator.initializeWithParser("/rules/community-rules.txt")

          println("Packet: "+packet)
          val gzipAsciiCompressed = Converter.ToGzipAscii(packet)
          val gzipPacketCompressed = new gzipPacket(gzipAsciiCompressed)

          val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacketCompressed, tcam = tcamSimulator)
          val algorithmResult = rtcamCompressedHttp.execute()

          resultsCompressed.append(algorithmResult)
          resultsNaive.append(algorithmResult)

        }

        printResults(resultsCompressed,resultsNaive)
      }

    test("Real html file ") {

      val tcamSimulators = new ListBuffer[tcamSimulator]
      val maxWidth = 50
      val jumps = 10
      for(w <- 10 to maxWidth by jumps){
        val tcamSimulator = new tcamSimulator(width = w)
        tcamSimulator.printTcam = false
        tcamSimulator.initializeWithParser("/rules/community-rules.txt")

        tcamSimulators.append(tcamSimulator)
      }


      val fileList = recursiveListFiles(new File("C:/Development/RTCAM-Compressed-HTTP/src/test/resources/realData/www.ynet.co.il/"));
      fileList.foreach((file) =>{
        if(file.isDirectory()){
          println(file.getAbsolutePath+" is directory")
        }
        else{

          for (ts <- 0 to tcamSimulators.length-1) {
            println("Processing file: " + file.getAbsolutePath)
            var path: String = file.getAbsolutePath
            //  path = path.replace("\\","/")
            val asStream: InputStream = new FileInputStream(file)
            val reader = new BufferedReader(new InputStreamReader(asStream))
            var payloadString = ""
            var line = reader.readLine();
            while (line != null) {
              println(line);
              line = reader.readLine();
              if (line != null) {
                payloadString += line
              }
            }
            val gzipAsciiCompressed = Converter.ToGzipAscii(payloadString)
            //println(String.format("Line: %s, GzipAcsii: %s", line, gzipAsciiCompressed))

            val gzipPacketCompressed = new gzipPacket(gzipAsciiCompressed)

            val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacketCompressed, tcam = tcamSimulators(ts))
            val algorithmResult = rtcamCompressedHttp.execute()
            println(algorithmResult)

            val gzipAsciiNaive = getNaiveGzipAsciiString(payloadString)
            val gzipPacketNaive = new gzipPacket(gzipAsciiNaive)
            val rtcamNaive = new rtcamCompressedHttp(packet = gzipPacketNaive, tcam = tcamSimulators(ts))
            val algorithmResultNaive = rtcamNaive.execute()
            println(algorithmResultNaive)
            writeResultsToFile(algorithmResult, algorithmResultNaive, file.getName)
          }
        }
      })

      /*println(payloadString)

      val gzipAsciiCompressed = Converter.ToGzipAscii(payloadString)
      val gzipPacketCompressed = new gzipPacket(gzipAsciiCompressed)

      val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacketCompressed, tcam = tcamSimulator)
      val algorithmResult = rtcamCompressedHttp.execute()*/


    }
  def getNaiveGzipAsciiString(payloadString:String) : String = {
    var retString = ""
    val toAscii =payloadString.map(_.toByte)
    toAscii.foreach(a => {
      retString+= "C"+a+";"
    })

    return retString
  }

  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def writeResultsToFile(algorithmResult:algorithmResult, algorithmResultNaive: algorithmResult ,fileName: String): Unit ={

    val skipAverageCompressed = ((algorithmResult.measurements.shiftSum + algorithmResult.measurements.numberOfCompressed) / algorithmResult.measurements.lookupCounter).toString
    val skipAverageNaive = ((algorithmResultNaive.measurements.shiftSum + algorithmResultNaive.measurements.numberOfCompressed) / algorithmResultNaive.measurements.lookupCounter).toString

    val lookupCsv = algorithmResult.measurements.packetLength.toString() + ","+algorithmResult.measurements.lookupCounter+","+algorithmResultNaive.measurements.lookupCounter+"\n"
    val fwLookup = new FileWriter("lookups.txt", true)
    try {
      fwLookup.write(lookupCsv)
    }
    finally fwLookup.close()

    val skipAvCsv = algorithmResult.measurements.tcamWidth.toString() + ","+skipAverageCompressed+","+skipAverageNaive+"\n"
    val fwSkipAv = new FileWriter("skipAverage.txt", true)
    try {
      fwSkipAv.write(skipAvCsv)
    }
    finally fwSkipAv.close()

    val fullDataCsv = fileName +","+ algorithmResult.measurements.packetLength.toString() +","+algorithmResult.measurements.lookupCounter +"|"+algorithmResultNaive.measurements.lookupCounter+","+ skipAverageCompressed+"|"+ skipAverageNaive + ","+algorithmResult.measurements.tcamWidth + "\n"
    val fwAllData = new FileWriter("fullResults.txt", true)
    try {
      fwAllData.write(fullDataCsv)
    }
    finally fwAllData.close()
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
      println("Number of matches: %s".format(resultsCompressed(i).matchList.length))
      println("")
    }
  }
}
