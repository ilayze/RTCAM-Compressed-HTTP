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
        var pos:Int = 0
        val n = packet.length

        while(pos <= n-width){
            val isPointer = packet.isPointer(pos,pos+width-1)
            if(isPointer)
              {
                //pointer handling
                pos = pos + 1
              }
            else{
              val key:String = packet.get(pos,pos+width-1)

              val entry = tcam.lookUp(key)
              val shift = entry.shift
              if(shift!=0){
                pos = pos+shift
              }
              else{

                if(entry.signatureLength<=width) {
                  matchedList.append(pos + entry.signatureLength)
                  pos = pos + 1
                }
                //check for match for signature greater than width
                else{
                  var checkingSignature = true //true as long as we check the current signature
                  var currentPos = pos + width  //current position in the checking
                  var alreadyChecked = width //number of characters of the current signature that we already checked

                  while(checkingSignature){
                    if(alreadyChecked+width<entry.signatureLength){
                      val currentKey:String = packet.get(currentPos,currentPos+width-1)

                      val currentEntry = tcam.lookUp(currentKey)
                      val currentShift = currentEntry.shift
                      if(currentShift!=0){
                        checkingSignature=false
                      }
                      else{
                        alreadyChecked = alreadyChecked+width
                        currentPos = currentPos+width
                        if(alreadyChecked.equals(entry.signatureLength))//match
                        {
                              checkingSignature=false
                        }
                      }
                    }else{
                      val charsToAdd = width-(entry.signatureLength-alreadyChecked)
                      val currentKey:String = packet.get(currentPos-charsToAdd,currentPos-charsToAdd+width-1)
                      val currentEntry = tcam.lookUp(currentKey)
                      val currentShift = currentEntry.shift
                      //match
                      if(currentShift==0){
                        checkingSignature=false
                        matchedList.append(pos + entry.signatureLength)
                        pos = pos + 1
                      }
                      else{
                        checkingSignature=false
                        pos = pos+1
                      }

                      }
                  }
                }
              }
            }
        }

        matchedList
      }
}
