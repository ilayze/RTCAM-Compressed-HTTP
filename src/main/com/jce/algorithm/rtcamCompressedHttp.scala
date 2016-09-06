package algorithm

import src.main.com.jce.{gzipPacket, rowMetadata, subPacket, tcamSimulator}

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/4/2016.
  */
class rtcamCompressedHttp(val packet: gzipPacket, val tcam: tcamSimulator) {

  var matchedList = new ListBuffer[Int]
  val width: Int = tcam.width
  var pos: Int = 0
  val n = packet.length
  var spmb = new Array[rowMetadata](n + width)
  //sub pattern match bit: if spmb(i)=true a sub pattern has been found in the index n the key that starts at index n-w returns shift = 0
  var pmb = new Array[rowMetadata](n + width) //pattern matching bit save a Boolean value for each bit bit[n] = true: a full pattern has been found in the index n
  var runtimeMeasurements = new runtimeMeasurements()
  println("Tcam width: %s, packet length: %s".format(width, n))

  def execute(): algorithmResult = {

    while (pos <= n - width) {
      val subPacket = packet.get(pos, pos + width - 1)
      val key: String = subPacket.data

      if (isInternalBoundary(subPacket)) {
        internalBoundaryHandler(subPacket)
      }
      else {
        val entry = tcam.lookUp(key)
        val shift = entry.shift
        runtimeMeasurements.shiftCounter+=1
        runtimeMeasurements.shiftSum+=shift
        if (shift != 0) {
          pos = pos + shift
        }
        else if (entry.signatureIndex != 0) //not a start of signature
        {
          pos += 1
        }
        else {
          checkMatch(entry)
        }
      }
    }

    return new algorithmResult(matchedList,runtimeMeasurements)

  }

  def isInternalBoundary(subPacket: subPacket): Boolean = {
    subPacket.pointerMetadata.isPointer &&
      subPacket.pointerMetadata.length > 2 * width - 2 &&
      subPacket.pointerMetadata.currentPos > width - 1 &&
      subPacket.pointerMetadata.currentPos < subPacket.pointerMetadata.length - width + 1
  }

  def checkMatch(entry: rowMetadata): Unit = {
    if (entry.signatureLength <= width) {
      matchLessThanWidth(entry)
    }
    //check for match for signature greater than width
    else {
      spmb(pos + width) = entry
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
          if (currentShift != 0 || currentEntry.signatureIndex != signatureIndex || currentEntry.signatureNumber != signatureNumber) {
            checkingSignature = false
            pos += 1
          }
          else {
            alreadyChecked = alreadyChecked + width
            currentPos = currentPos + width
            signatureIndex += 1
          }
        } else {
          val charsToAdd = width - (entry.signatureLength - alreadyChecked) //end of the signature we might need to add some chars e.g signature abcd width 3 so will check abc and then bcd so we added bc in the second check
          if (currentPos - charsToAdd + width - 1 >= n) {
            checkingSignature = false
            pos = pos + 1
          }
          else {
            val currentSubPacket = packet.get(currentPos - charsToAdd, currentPos - charsToAdd + width - 1)
            val currentKey: String = currentSubPacket.data
            val currentEntry = tcam.lookUp(currentKey)
            val currentShift = currentEntry.shift
            //match
            if (currentShift == 0 && currentEntry.signatureIndex == signatureIndex && currentEntry.signatureNumber == signatureNumber) {
              checkingSignature = false
              val signature_pos = pos + entry.signatureLength
              println("Match!!! pos: " + signature_pos.toString())
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

  def matchLessThanWidth(entry: rowMetadata): Unit = {
    val signature_pos = pos + entry.signatureLength
    println("Match!!! pos: " + signature_pos.toString())
    matchedList.append(signature_pos)
    spmb(signature_pos) = entry
    pmb(signature_pos) = entry
    pos = pos + 1
  }

  def internalBoundaryHandler(subPacket: subPacket): Unit = {
    println("internal boundary")

    for (i <- pos + width until (pos + subPacket.pointerMetadata.length - width)) {
      val pmbIndex = i - subPacket.pointerMetadata.distance
      if (pmb(pmbIndex) != null) //check if the rest of the signature match
      {
        var checkingInternalMatch = true
        val sigNumber = pmb(pmbIndex).signatureNumber
        var sigIndex = pmb(pmbIndex).signatureIndex
        var counter = 1
        while (checkingInternalMatch) {
          if (sigIndex == 0) {
            checkingInternalMatch = false
            val sig_pos = i
            println("Match!!! pos: " + sig_pos.toString())
            matchedList.append(sig_pos)
          }
          else if (spmb(pmbIndex - counter * width) != null) {
            if (spmb(pmbIndex - counter * width).signatureNumber == sigNumber && spmb(pmbIndex - counter * width).signatureIndex == sigIndex - 1) {
              sigIndex = sigIndex - 1
              counter += 1
            }
          }
          else {
            checkingInternalMatch = false
          }
        }
      }
    }

    val incrementPos = (pos + subPacket.pointerMetadata.length - width) - (pos + width)
    pos += incrementPos
  }
}

class algorithmResult(val matcheList: ListBuffer[Int],val measurments:runtimeMeasurements)

