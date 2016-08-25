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
      for(i <- keyLength to 0 by -1)
      {
          var keyWithDontCare = key.substring(0,i)
          val numberOfDontCare:Int = key.length()-i-1
          for(dc <- numberOfDontCare to 0 by -1)
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

          if(signature!= null && signature.length()<=width)
          {
            val length = signature.length();
            var i = 0
            for(i <- 0 until length)
            {
                var signature_with_dont_care = signature.substring(0,length-i)
                var j=0
                for(j <-0 until i)
                  signature_with_dont_care = signature_with_dont_care.concat(DONT_CARE)

                val newRow = new row(signature_with_dont_care)
                val newRowMewtadata = new rowMetadata(shift = i)
                val newTcamEntry = new tcamEntry(newRow,newRowMewtadata)
                println("new tcam entry: %s".format(newTcamEntry))
                addEntry(newTcamEntry)
            }

            addEntry(new tcamEntry(new row(dontcare(signature.length())),new rowMetadata(shift = signature.length())))
          }
    }
}

class rowMetadata(val shift:Int)

class row(val data:String)

class tcamEntry(val row:row, val metadata:rowMetadata)
{
  override def toString: String = {
    println("data: %s, shift: %s".format(row.data, metadata.shift))
    return ""
  }

}
