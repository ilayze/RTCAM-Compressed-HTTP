package algorithm

import src.main.com.jce.{gzipPacket, tcamSimulator}

/**
  * Created by izeidman on 8/4/2016.
  */
class rtcamCompressedHttp(val packet:gzipPacket,val tcam:tcamSimulator) {
      def execute(): Unit ={
        val width:Int = tcam.width
        var pos:Int = 1
        val n = packet.length

        while(pos <= n-width){
            val key:String = packet.get(pos,pos+width-1)
            val entry = tcam.lookUp(key)
            val shift = entry.shift
            if(shift!=0){
                pos = pos+shift
            }
          else{

            }
        }

        println("TCAM width: %s".format(width))



      }
}
