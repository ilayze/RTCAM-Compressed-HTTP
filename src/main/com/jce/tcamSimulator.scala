package src.main.com.jce

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/4/2016.
  *
  * Simulator for Ternary content-addressable memory(TCAM) see: https://en.wikipedia.org/wiki/Content-addressable_memory#Ternary_CAMs
  */
class tcamSimulator(val width:Int) {
    var tcam = new ListBuffer[tcamEntry]
    val DONT_CARE = "xdc"

    def lookUp(key: String): rowMetadata =
    {

      val keyLength = key.length()
      for(i <- 0 to keyLength)
      {
          var keyWithDontCare = key.substring(i,keyLength)
          val numberOfDontCare:Int = i
          for(dc <- numberOfDontCare to 1 by -1)
            keyWithDontCare =keyWithDontCare+DONT_CARE
         val entry =getEntry(keyWithDontCare)
          if(entry!=null)
            return entry
      }

        throw new Exception("No row match key "+key)
    }

    def getEntry(key: String): rowMetadata =
    {
      for(entry <- tcam){
        if(entry.row.data==key)
          return entry.metadata
      }
      return null
    }

    def addEntry(entry: tcamEntry): Unit =
    {
        tcam.append(entry)
        println("new tcam entry: %s".format(entry))
    }

  def dontcare(numberOfDc: Int): String = {
    var dontcares = "";
    for (i<- 1 to numberOfDc){
      dontcares = dontcares.concat(DONT_CARE)
    }
    return dontcares
  }

  //initialize the tcam with one signature
    def initialize(signature : String): Unit ={

          if(signature==null)
            throw new Exception("Signature is null")

          if(signature.length()<=width)
          {
            val length = signature.length()
            var i = 0
            for(i <- 0 until length)
            {
                var signature_with_dont_care = signature.substring(0,length-i)
                var j=0
                for(j <-0 until width-signature_with_dont_care.length())
                  signature_with_dont_care = signature_with_dont_care.concat(DONT_CARE)

                val newRow = new row(signature_with_dont_care)
                val newRowMewtadata = new rowMetadata(shift = i,signatureLength = length)
                val newTcamEntry = new tcamEntry(newRow,newRowMewtadata)
                addEntry(newTcamEntry)
            }

            addEntry(new tcamEntry(new row(dontcare(width)),new rowMetadata(shift = width,signatureLength = width)))
          }
          else{
            throw new Exception("Method not implemented")
          }

    }
}

class rowMetadata(val shift:Int,val signatureLength:Int)

class row(val data:String)

class tcamEntry(val row:row, val metadata:rowMetadata)
{
  override def toString: String = {
    val ret="data: %s, shift: %s".format(row.data, metadata.shift)
    return ret
  }

}
