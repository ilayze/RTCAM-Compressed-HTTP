package algorithm

/**
  * Created by izeidman on 9/6/2016.
  */
class runtimeMeasurements(val packetLength:Int,val tcamWidth:Int,val numberOfUncompressed:Int) {
  var lookupCounter = 0
  var shiftSum = 0
  var skipSum = 0
  var memoryAccessCounter = 0
  var actualScannedBytes = 0
  var numberOfCompressed = packetLength-numberOfUncompressed

  def compressionRatio = numberOfUncompressed/packetLength
  def scannedRatio = actualScannedBytes/packetLength

  def printMeasurements: Unit = {
    println("\n##################### Measurements #####################")

    println("%s lookups, %s memory access, packet length %s, tcam width %s, number of uncompressed %s, number of compressed %s".format(lookupCounter,memoryAccessCounter, packetLength, tcamWidth,numberOfUncompressed,numberOfCompressed))
    if (lookupCounter > 0)
      println("Shift average: %s".format(shiftSum / lookupCounter))

    println("##################### End Of Measurements #####################")
  }
}
