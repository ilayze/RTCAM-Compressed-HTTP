package algorithm

import src.main.com.jce._

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/4/2016.
  */
class rtcamCompressedHttp(val packet: gzipPacket, val tcam: tcamSimulator) {

  var matchedList = new ListBuffer[Int]
  val width: Int = tcam.width
  var pos: Int = 0
  val n = packet.length
  var spmb = new Array[ListBuffer[subSignatureMetadata]](n + width)
  var pmb = new Array[ListBuffer[subSignatureMetadata]](n + width)
  var runtimeMeasurements = new runtimeMeasurements(packetLength = n, tcamWidth = width)
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
        val shift = entry(0).shift
        runtimeMeasurements.lookupCounter += 1
        runtimeMeasurements.shiftSum += shift
        if (shift != 0) {
          pos = pos + shift
        }
        else {
          checkMatch(entry)
        }
      }
    }

    printMeasurements
    return new algorithmResult(matchedList, runtimeMeasurements)

  }

  def isInternalBoundary(subPacket: subPacket): Boolean = {
    subPacket.pointerMetadata.isPointer &&
      subPacket.pointerMetadata.length > 2 * width - 2 &&
      subPacket.pointerMetadata.currentPos > width - 1 &&
      subPacket.pointerMetadata.currentPos < subPacket.pointerMetadata.length - width + 1
  }

  def internalBoundaryHandler(subPacket: subPacket): Unit = {
    println("internal boundary")

    for (i <- pos + width - 1 until (pos + subPacket.pointerMetadata.length - width)) {
      val pmbIndex = i - subPacket.pointerMetadata.distance
      if (pmb(pmbIndex) != null) //check if the rest of the signature match
      {
        for (subSig <- pmb(pmbIndex)) {
          var checkingInternalMatch = true
          val sigNumber = subSig.signatureNumber
          var sigIndex = subSig.signatureIndex
          var counter = 1
          while (checkingInternalMatch) {
            if (sigIndex == 0) {
              checkingInternalMatch = false
              val sig_pos = i
              println("Match!!! pos: " + sig_pos.toString())
              matchedList.append(sig_pos)
            }
            else if (spmb(pmbIndex - counter * width) != null) {
              for (spmbSubSig <- spmb(pmbIndex - counter * width)) {
                if (spmbSubSig.signatureNumber == sigNumber && spmbSubSig.signatureIndex == sigIndex - 1) {
                  sigIndex = sigIndex - 1
                  counter += 1
                }
              }
            }
            else {
              checkingInternalMatch = false
            }
          }
        }
      }
    }

    val incrementPos = subPacket.pointerMetadata.length - 2 * width + 1
    pos += incrementPos
  }

  /**
    * Iterate through all the input sub signatures and for each
    * check if there is a full match of a pattern
    */
  def checkMatch(subSignaturesMetadata: ListBuffer[subSignatureMetadata]): Unit = {

    for (subSigMetadata <- subSignaturesMetadata) {
      //there is a match because the shift is 0 and the signature length is <= width
      if (subSigMetadata.signatureLength <= width) {
        matchLessThanWidth(subSigMetadata)
        spmb(pos - 1 + subSigMetadata.signatureLength) = subSignaturesMetadata
        pmb(pos - 1 + subSigMetadata.signatureLength) = subSignaturesMetadata
      }
      //check for match for signature greater than width
      else {
        if (pos + width < spmb.length)
          spmb(pos + width) = subSignaturesMetadata
        var checkingSignature = true //true as long as we check the current signature
        var currentPos = pos + width //current position in the checking
        var alreadyChecked = width //number of characters of the current signature that we already checked
        var signatureIndex = 1
        var signatureNumber = subSigMetadata.signatureNumber

        while (checkingSignature) {
          //there are still more than width chars to check in the current signature
          if (alreadyChecked + width < subSigMetadata.signatureLength) {
            if (currentPos <= n - width) {
              val subPacket = packet.get(currentPos, currentPos + width - 1)
              val currentKey = subPacket.data

              val currentEntry = tcam.lookUp(currentKey)
              val currentShift = currentEntry(0).shift
              checkingSignature = false
              for (currentSig <- currentEntry) {
                if (currentShift != 0 || currentSig.signatureIndex != signatureIndex || currentSig.signatureNumber != signatureNumber) {

                }
                else {
                  alreadyChecked = alreadyChecked + width
                  currentPos = currentPos + width
                  signatureIndex += 1
                  checkingSignature = true
                }
              }
              if (!checkingSignature)
                pos += 1
            } else {
              pos += 1
              checkingSignature = false
            }
            //check the end of the signature
          } else {
            val charsToAdd = width - (subSigMetadata.signatureLength - alreadyChecked) //end of the signature we might need to add some chars e.g signature abcd width 3 so will check abc and then bcd so we added bc in the second check
            if (currentPos - charsToAdd + width - 1 >= n) {
              checkingSignature = false
              pos = pos + 1
            }
            else {
              val currentSubPacket = packet.get(currentPos - charsToAdd, currentPos - charsToAdd + width - 1)
              val currentKey: String = currentSubPacket.data
              val currentEntry = tcam.lookUp(currentKey)
              val currentShift = currentEntry(0).shift

              for (s <- currentEntry) {
                if (currentShift == 0 && s.signatureIndex == signatureIndex && s.signatureNumber == signatureNumber) {
                  val signature_pos = pos + s.signatureLength
                  println("Match!!! pos: " + signature_pos.toString())
                  matchedList.append(signature_pos)
                  spmb(signature_pos) = currentEntry
                  pmb(signature_pos) = currentEntry
                }
              }
              checkingSignature = false
              pos = pos + 1
            }
          }
        }
      }
    }
  }

  def matchLessThanWidth(entry: subSignatureMetadata): Unit = {
    val signature_pos = pos + entry.signatureLength
    println("Match!!! pos: " + signature_pos.toString())
    matchedList.append(signature_pos)
    pos = pos + 1
  }

  def printMeasurements: Unit = {
    println("\n##################### Measurements #####################")

    println("%s lookups, packet length %s, tcam width %s".format(runtimeMeasurements.lookupCounter, runtimeMeasurements.packetLength, runtimeMeasurements.tcamWidth))
    if (runtimeMeasurements.lookupCounter > 0)
      println("Shift average: %s".format(runtimeMeasurements.shiftSum / runtimeMeasurements.lookupCounter))

    println("##################### End Of Measurements #####################")
  }
}

class algorithmResult(val matchList: ListBuffer[Int], val measurements: runtimeMeasurements)

