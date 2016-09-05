package algorithm

import src.main.com.jce.{rowMetadata, gzipPacket, tcamSimulator}

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
    var spmb = new Array[rowMetadata](n+width) //sub pattern match bit: if spmb(i)=true a sub pattern has been found in the index n the key that starts at index n-w returns shift = 0
    var pmb = new Array[rowMetadata](n+width) //pattern matching bit save a Boolean value for each bit bit[n] = true: a full pattern has been found in the index n

    println("Tcam width: %s, packet length: %s".format(width,n))

    while (pos <= n- width) {

      val subPacket = packet.get(pos, pos + width - 1)
      val key: String = subPacket.data
      //internal boundary - should not lookup in tcam
      if(subPacket.pointerMetadata.isPointer && subPacket.pointerMetadata.length>2*width-2 && subPacket.pointerMetadata.currentPos>width-1 && subPacket.pointerMetadata.currentPos<subPacket.pointerMetadata.length-width+1)
        {
          println("internal boundary")
          for(i<-pos+width until(pos+subPacket.pointerMetadata.length-width)){
            val pmbIndex = i-subPacket.pointerMetadata.distance
            if(pmb(pmbIndex)!=null) //check if there is an internal match
            {
                var checkingInternalMatch = true
                val sigNumber = pmb(pmbIndex).signatureNumber
                var sigIndex = pmb(pmbIndex).signatureIndex
                var counter =1
                while (checkingInternalMatch){
                    if(sigIndex==0)
                      {
                        checkingInternalMatch = false
                        val sig_pos = i
                        println("Match!!! pos: "+sig_pos.toString())
                        matchedList.append(sig_pos)
                      }
                    else if(spmb(pmbIndex-counter*width)!=null){
                      if(spmb(pmbIndex-counter*width).signatureNumber==sigNumber && spmb(pmbIndex-counter*width).signatureIndex==sigIndex-1){
                        sigIndex=sigIndex-1
                        counter+=1
                      }
                    }
                    else{
                      checkingInternalMatch=false
                    }
                }
            }
          }

          val incrementPos = (pos+subPacket.pointerMetadata.length-width)-(pos+width)
          pos+= incrementPos

        }

      val entry = tcam.lookUp(key)
      val shift = entry.shift
      if (shift != 0) {
        pos = pos + shift
      }
      else if(entry.signatureIndex!=0) //not a start of signature
      {
        pos+=1
      }
      else {

        if (entry.signatureLength <= width) {
          val signature_pos = pos + entry.signatureLength
          println("Match!!! pos: "+signature_pos.toString())
          matchedList.append(signature_pos)
          spmb(signature_pos)=entry
          pmb(signature_pos)=entry
          pos = pos + 1
        }
        //check for match for signature greater than width
        else {
          spmb(pos+width)=entry
          var checkingSignature = true //true as long as we check the current signature
          var currentPos = pos + width //current position in the checking
          var alreadyChecked = width //number of characters of the current signature that we already checked
          var signatureIndex = 1
          var signatureNumber = entry.signatureNumber

          while (checkingSignature) {
            if (alreadyChecked + width < entry.signatureLength) {
              val subPacket = packet.get(currentPos, currentPos + width - 1)
              val currentKey = subPacket.data

              val currentEntry = tcam.lookUp(currentKey)
              val currentShift = currentEntry.shift
              if (currentShift != 0 || currentEntry.signatureIndex!=signatureIndex || currentEntry.signatureNumber!=signatureNumber) {
                checkingSignature = false
                pos+=1
              }
              else {
                alreadyChecked = alreadyChecked + width
                currentPos = currentPos + width
                signatureIndex+=1
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
                if (currentShift == 0 && currentEntry.signatureIndex==signatureIndex && currentEntry.signatureNumber==signatureNumber) {
                  checkingSignature = false
                  val signature_pos = pos + entry.signatureLength
                  println("Match!!! pos: "+signature_pos.toString())
                  matchedList.append(signature_pos)
                  spmb(signature_pos) = currentEntry
                  pmb(signature_pos) = currentEntry
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
