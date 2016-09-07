package src.main.com.jce

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/4/2016.
  */
class gzipPacket(val data: String) {

  val dataSplitted: Array[String] = data.split(";")
  var dataSplittedFull = new ListBuffer[String]
  var dataSplittedMetadata = new ListBuffer[pointerMetadata]

  for (i <- 0 until (dataSplitted.length)) {
    if (dataSplitted(i).startsWith("C")) {
      dataSplittedFull.append(dataSplitted(i))
      dataSplittedMetadata.append(new notPointer())
    }
    else if (dataSplitted(i).startsWith("L")) {
      val pointerLength = getPointerValue(dataSplitted(i)) //length of the pointer
      val pointerDistance = getPointerValue(dataSplitted(i + 1)) //distnace to go back to start the pointer from

      for (j <- 0 until (pointerLength)) {
        dataSplittedFull.append(dataSplitted(i - pointerDistance + j%pointerDistance))
        dataSplittedMetadata.append(new pointerMetadata(currentPos = j + 1, isPointer = true, length = pointerLength, distance = pointerDistance))
      }


    }
  }

  def get(begin: Int, end: Int): subPacket = {
    println("Gzip packet get(%d,%d)".format(begin, end))

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
