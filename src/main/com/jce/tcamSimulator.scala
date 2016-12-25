package src.main.com.jce

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/4/2016.
  *
  * Simulator for Ternary content-addressable memory(TCAM) see: https://en.wikipedia.org/wiki/Content-addressable_memory#Ternary_CAMs
  */
class tcamSimulator(val width: Int) {
  var tcam = new ListBuffer[tcamEntry]
  val DONT_CARE = "z"
  var signatureNumber = 1
  var printTcam = true
  var lookupCounter = 0
  var shiftSum = 0
  var shiftsHistory = new ListBuffer[Int]
  var matchIdexes = new ListBuffer[Int]


  def lookUp(key: String,lookupType:String = "start"): ListBuffer[subSignatureMetadata] = {
    println("Tcam simulator lookup key: %s".format(key))
    val keyHex = snortToHex(key)
    return lookupHex(keyHex)

  }


  def lookupHex(key: String): ListBuffer[subSignatureMetadata] = {
    val keyLength = key.length()
    for (i <- 0 to keyLength by 2) {
      var subkeyWithDC = key.substring(i, keyLength)
      val numberOfDontCare: Int = (width*2 - subkeyWithDC.length())/2
      for (dc <- numberOfDontCare to 1 by -1)
        subkeyWithDC = DONT_CARE + subkeyWithDC
      val entry = getEntry(subkeyWithDC)
      if (entry != null) {
        println("Tcam simulator lookup return entry with shift: %d".format(entry(0).shift))
        lookupCounter += 1
        shiftSum += entry(0).shift
        shiftsHistory.append(entry(0).shift)
        return entry
      }
    }

    throw new Exception("No row match key " + key)
  }

  def getEntry(key: String): ListBuffer[subSignatureMetadata] = {
    for (entry <- tcam) {
      if (entry.row.data == key)
        return entry.metadata
        //match with dont cares e.g key: abcde entry: abcd?
      else if (entry.row.data.endsWith(DONT_CARE) && DONT_CARE.r.findAllMatchIn(entry.row.data).length < width) {
        var moreDC = true
        var tcamData = entry.row.data
        var keyChanged = key
        while (moreDC) {
          if (tcamData.endsWith(DONT_CARE)) {
            tcamData = tcamData.substring(0, tcamData.length() - DONT_CARE.length())
            keyChanged = keyChanged.substring(0, keyChanged.length() - 2)
            println("Tcam data:"+tcamData+", key:"+keyChanged+":"+(keyChanged.length() - 2).toString)
            if (tcamData.equals(keyChanged))
              return entry.metadata
          } else {
            moreDC = false
          }
        }
      }
      //todo add case where the key starts with dont cares! see lookupHex function
    }
    return null
  }

  def addEntry(row: row, subSignatureMetadata: subSignatureMetadata): Unit = {
    //check if the entry already exists
    for (e <- tcam) {
      if (e.row.data == row.data) {
        e.metadata.append(subSignatureMetadata)
        return
      }

    }

    val subSigToAdd = new ListBuffer[subSignatureMetadata]()
    subSigToAdd.append(subSignatureMetadata)
    val entry = new tcamEntry(row = row, metadata = subSigToAdd)
    tcam.append(entry)
    println("new tcam entry: %s".format(entry))
  }

  def dontcare(numberOfDc: Int): String = {
    var dontcares = "";
    for (i <- 1 to numberOfDc) {
      dontcares = dontcares.concat(DONT_CARE)
    }
    return dontcares
  }

  //initialize the tcam with one signature
  def initialize(signature: String): Unit = {

    if (signature == null)
      throw new Exception("Signature is null")

    val hexSignature = snortToHex(signature)
    initializeHex(hexSignature)

  }

  def initializeHex(hexSignature: String): Unit = {

    val hexWidth = width*2
    var signatureSplitted = hexSignature.grouped(hexWidth).toList
    if (signatureSplitted.last.length() != hexWidth && signatureSplitted.length > 1) {
      val suffix = hexSignature.substring(hexSignature.length() - hexWidth, hexSignature.length())
      signatureSplitted = signatureSplitted.updated(signatureSplitted.length - 1, suffix)
    }

    for (itemIndex <- 0 until (signatureSplitted.length)) {
      var itemWithDC = signatureSplitted(itemIndex)
      var numberOfRightDC = 0
      if (itemWithDC.length() < hexWidth) {
        //add don't cares to the right
        for (n <- 0 until (hexWidth - itemWithDC.length()) by 2) {
          itemWithDC = itemWithDC + DONT_CARE
          numberOfRightDC += 1
        }
      }

      var i = 0
      for (i <- 0 until width) {
        var numberOfCharacters = 0
        for (m <- 0 until (i)) {
          if (m < numberOfRightDC)
            numberOfCharacters += DONT_CARE.length()
          else {
            numberOfCharacters += 2
          }
        }

        var signature_with_dont_care = itemWithDC.substring(0, itemWithDC.length() - numberOfCharacters)
        var j = 0
        for (j <- 0 until i)
          signature_with_dont_care = DONT_CARE + signature_with_dont_care

        val newRow = new row(signature_with_dont_care)
        val newRowMewtadata = new subSignatureMetadata(shift = i, signatureLength = hexSignature.length()/2, signatureNumber = signatureNumber, signatureIndex = itemIndex)
        addEntry(newRow, newRowMewtadata)
      }
    }

    addEntry(new row(dontcare(width)), new subSignatureMetadata(shift = width, signatureLength = width, signatureNumber = -1, signatureIndex = -1))
    if (printTcam)
      println("############# TCAM entries #############\n" + this.toString())

    signatureNumber += 1
  }

  def initializeWithParser(rules_file_path: String): Unit = {
    val parser = new ruleParser()
    val signatures = parser.parseRules(rules_file_path)
    for (sig <- signatures) {
      initialize(sig.regular)
    }
  }

  /*
  *  Takes String with snort representation and convert it to hex e.g 2|00 00 00 06 00 00 00|Drives|24 00| every | ... | is alredy in hex representation and what outside it converted into hex
  * */
  def snortToHex(input: String) : String = {
    var ret = ""
    val splittedInput = input.split('|')
    for (i<-0 until(splittedInput.length)){
      if(i%2==0) //regular string covert to hex
      {
        val toHex = string2hex(splittedInput(i))
        ret = ret.concat(toHex)
      }
      else // already in hex represenation e.g 00 00 00 06 00 00 00
      {
        ret = ret.concat(splittedInput(i).replaceAll(" ",""))
      }
    }

    return ret
  }

  def string2hex(str: String): String = {
    str.toList.map(_.toInt.toHexString).mkString
  }

  override def toString(): String = {
    var ret = ""
    for (i <- 0 until ((width + 1))) {
      for (entry <- tcam) {
        if (entry.metadata(0).shift.equals(i)) {
          val entryData = entry.row.data.replace(DONT_CARE, "?")
          ret = ret + entryData + "\n"
        }
      }
    }
    ret
  }
}

class subSignatureMetadata(val shift: Int, val signatureLength: Int, val signatureNumber: Int, val signatureIndex: Int)

class row(val data: String)

class tcamEntry(val row: row, val metadata: ListBuffer[subSignatureMetadata]) {
  override def toString: String = {
    var ret = ""
    for (subSig <- metadata) {
      ret += "data: %s, shift: %s, index: %s, number: %s, length: %s".format(row.data, subSig.shift, subSig.signatureIndex, subSig.signatureNumber, subSig.signatureLength)
    }
    return ret
  }

}

