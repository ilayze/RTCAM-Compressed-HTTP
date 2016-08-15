package algorithm

import src.main.com.jce.{gzipPacket, tcamSimulator}

/**
  * Created by izeidman on 8/4/2016.
  */
class rtcamCompressedHttp(val packet:gzipPacket,val tcam:tcamSimulator) {
      def execute(): Unit ={
        val width = tcam.width
        println("TCAM width: %s".format(width))

        

      }
}
