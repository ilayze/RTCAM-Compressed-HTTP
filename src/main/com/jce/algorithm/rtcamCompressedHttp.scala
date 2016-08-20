package algorithm

import src.main.com.jce.{gzipPacket, tcamSimulator}

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/4/2016.
  */
class rtcamCompressedHttp(val packet:gzipPacket,val tcam:tcamSimulator) {
      def execute(): ListBuffer[Int] ={
        var matchedList = new ListBuffer[Int]
        val width:Int = tcam.width
        var pos:Int = 1
        val n = packet.length

        while(pos <= n-width){
            val key:Array[String] = packet.get(pos,pos+width-1)
            val entry = tcam.lookUp(key.toString)
            val shift = entry.shift
            if(shift!=0){
                pos = pos+shift
            }
          else{
              pos = pos +1
            }
        }

        println("TCAM width: %s".format(width))
        matchedList
      }
}
