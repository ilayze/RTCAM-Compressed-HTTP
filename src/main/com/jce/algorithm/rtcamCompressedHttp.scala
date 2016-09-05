package algorithm

import src.main.com.jce.{gzipPacket, tcamSimulator}

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/4/2016.
  */
class rtcamCompressedHttp(val packet: gzipPacket, val tcam: tcamSimulator) {
  def execute(): ListBuffer[Int] = {
    var matchedList = new ListBuffer[Int]
    val width: Int = tcam.width
    var pos: Int = 0
    val n = packet.length
    var spmb = new Array[Boolean](n+width) //sub pattern match bit: if spmb(i)=true a sub pattern has been found in the index n the key that starts at index n-w returns shift = 0
    var pmb = new Array[Boolean](n+width) //pattern matching bit save a Boolean value for each bit bit[n] = true: a full pattern has been found in the index n

    while (pos <= n- width) {

      val subPacket = packet.get(pos, pos + width - 1)
      val key: String = subPacket.data
      //internal boundary - should not lookup in tcam
      if(subPacket.pointerMetadata.isPointer && subPacket.pointerMetadata.length>2*width-2 && subPacket.pointerMetadata.currentPos-width>width-1 && subPacket.pointerMetadata.currentPos<subPacket.pointerMetadata.length-width+1)
        {
          println("internal boundary")
          throw new NotImplementedError("Not implemented yet!")
        }

      val entry = tcam.lookUp(key)
      val shift = entry.shift
      if (shift != 0) {
        pos = pos + shift
      }
      else {

        if (entry.signatureLength <= width) {
          val signature_pos = pos + entry.signatureLength
          println("Match!!! pos: "+signature_pos.toString())
          matchedList.append(signature_pos)
          spmb(signature_pos)=true
          pmb(signature_pos)=true
          pos = pos + 1
        }
        //check for match for signature greater than width
        else {
          spmb(pos+width)=true
          var checkingSignature = true //true as long as we check the current signature
          var currentPos = pos + width //current position in the checking
          var alreadyChecked = width //number of characters of the current signature that we already checked

          while (checkingSignature) {
            if (alreadyChecked + width < entry.signatureLength) {
              val subPacket = packet.get(currentPos, currentPos + width - 1)
              val currentKey = subPacket.data

              val currentEntry = tcam.lookUp(currentKey)
              val currentShift = currentEntry.shift
              if (currentShift != 0) {
                checkingSignature = false
                pos+=1
              }
              else {
                alreadyChecked = alreadyChecked + width
                currentPos = currentPos + width
              }
            } else {
              val charsToAdd = width - (entry.signatureLength - alreadyChecked) //end of the signature we might need to add some chars e.g signature abcd width 3 so will check abc and then bcd so we added bc in the second check
              if(currentPos - charsToAdd + width - 1>=n)
                {
                  checkingSignature=false
                  pos = pos+1
                }
              else{
                val currentSubPacket = packet.get(currentPos - charsToAdd, currentPos - charsToAdd + width - 1)
                val currentKey: String = currentSubPacket.data
                val currentEntry = tcam.lookUp(currentKey)
                val currentShift = currentEntry.shift
                //match
                if (currentShift == 0) {
                  checkingSignature = false
                  val signature_pos = pos + entry.signatureLength
                  println("Match!!! pos: "+signature_pos.toString())
                  matchedList.append(signature_pos)
                  spmb(signature_pos) = true
                  pmb(signature_pos) =true
                  pos = pos + 1
                }
                else {
                  checkingSignature = false
                  pos = pos + 1
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
