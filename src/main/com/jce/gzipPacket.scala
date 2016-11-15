package src.main.com.jce

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/4/2016.
  */
class gzipPacket(val data: String) {

  val dataSplitted: Array[String] = data.split(";")
  var dataSplittedFull = new ListBuffer[String]
  var dataSplittedMetadata = new ListBuffer[pointerMetadata]
  var accessNumber = 0

  for (i <- 0 until (dataSplitted.length)) {
    if (dataSplitted(i).startsWith("C")) {
      dataSplittedFull.append(dataSplitted(i))
      dataSplittedMetadata.append(new notPointer())
    }
    else if (dataSplitted(i).startsWith("L")) {
      val pointerLength = getPointerValue(dataSplitted(i)) //length of the pointer
      val pointerDistance = getPointerValue(dataSplitted(i + 1)) //distnace to go back to start the pointer from

      var pointerInPointerLength = 0
      for (curr <- 0 until (pointerLength)) {
        val index = i - curr
        if (index >= 0) {
          //check if there is pointer within pointer
          if (dataSplitted(index).startsWith("D")) {
            val lNum = getPointerValue(dataSplitted(index - 1))
            pointerInPointerLength += lNum - 2
          }
        }
      }

      for (j <- 0 until (pointerLength)) {
        dataSplittedFull.append(dataSplittedFull(i - pointerDistance + j % pointerDistance + pointerInPointerLength))
        dataSplittedMetadata.append(new pointerMetadata(currentPos = j + 1, isPointer = true, length = pointerLength, distance = pointerDistance))
      }


    }
  }

  def get(begin: Int, end: Int): subPacket = {
    println("Gzip packet get(%d,%d)".format(begin, end))
    accessNumber += 1

    var data = ""
    for (i <- begin until (end + 1)) {
      data += dataSplittedFull(i).substring(1).toInt.toChar
    }
    val metadata = dataSplittedMetadata(end)

    println("Gzip packet get return data: %s, %s".format(data, metadata))
    return new subPacket(data = data, pointerMetadata = metadata)

  }

  def isPointer(begin: Int, end: Int): Boolean = {
    val dataSplittedSlice = dataSplitted.slice(begin, end + 1)
    for (i <- dataSplittedSlice) {
      if (i.contains("L") || i.contains("D"))
        return true
    }
    return false
  }

  def length: Int = {
    var length: Int = 0
    for (i <- dataSplitted) {
      if (i.startsWith("C")) {
        length += 1
      }
      else if (i.startsWith("L")) {
        length += getPointerValue(i)
      }
    }
    return length
  }

  def getPointerValue(i: String): Int = {
    i.substring(1, i.length()).toInt
  }

  def getNumberOfUncompressed:Int = {
    var ret = 0
    for (i <- dataSplitted) {
      if (i.startsWith("C")) {
        ret += 1
      }
    }
    ret
  }

  override def toString(): String = {
    var ret="Full Packet: "
    for(i<-0 until(dataSplittedFull.length)){
      ret += dataSplittedFull(i).substring(1).toInt.toChar
    }
    ret+="\nCompressed: "
    for(i<-0 until(dataSplitted.length)){
      if (dataSplitted(i).startsWith("C")) {
          ret += dataSplitted(i).substring(1).toInt.toChar
      }
      else if (dataSplitted(i).startsWith("L")) {
          ret += "["+dataSplitted(i).substring(1)+","+dataSplitted(i+1).substring(1)+"]"
      }
    }
    ret
  }


}

class pointerMetadata(val length: Int, val currentPos: Int, val isPointer: Boolean, val distance: Int) {
  override def toString(): String = {
    return "pointerMetadata: isPointer: %s, currentPos: %d, pointerLength: %d, distance: %d".format(isPointer, currentPos, length, distance)
  }
}

class notPointer extends pointerMetadata(length = -1, currentPos = -1, isPointer = false, distance = -1) {
  override def toString(): String = {
    return "no pointer"
  }
}

class subPacket(var data: String, var pointerMetadata: pointerMetadata)
