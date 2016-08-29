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
          var subkeyWithDC = key.substring(i,keyLength)
          val numberOfDontCare:Int = width - subkeyWithDC.length()
          for(dc <- numberOfDontCare to 1 by -1)
            subkeyWithDC =DONT_CARE+subkeyWithDC
         val entry =getEntry(subkeyWithDC)
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
      //check if the entry already exists
      for(e <- tcam){
        if(e.row.data==entry.row.data)
          return
      }

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

          var signatureSplitted = signature.grouped(width).toList
          if(signatureSplitted.last.length()!=width && signatureSplitted.length>1) {
            val suffix = signature.substring(signature.length()-width,signature.length())
            signatureSplitted = signatureSplitted.updated(signatureSplitted.length-1,suffix)
          }

          for (item <- signatureSplitted)
          {
            var itemWithDC = item
            var numberOfRightDC = 0
            if(itemWithDC.length()<width){ //add don't cares to the right
              for(n<-0 until(width-itemWithDC.length())){
                itemWithDC = itemWithDC+DONT_CARE
                numberOfRightDC+=1
              }
            }

            var i = 0
            for(i <- 0 until width)
            {
              var numberOfCharacters = 0
              for(m<-0 until(i)){
                if(m<numberOfRightDC)
                  numberOfCharacters+=DONT_CARE.length()
                else{
                  numberOfCharacters+=1
                }
              }

              var signature_with_dont_care = itemWithDC.substring(0,itemWithDC.length()-numberOfCharacters)
              var j=0
              for(j <-0 until i)
                signature_with_dont_care = DONT_CARE+signature_with_dont_care

              val newRow = new row(signature_with_dont_care)
              val newRowMewtadata = new rowMetadata(shift = i,signatureLength = signature.length())
              val newTcamEntry = new tcamEntry(newRow,newRowMewtadata)
              addEntry(newTcamEntry)
            }
          }

          addEntry(new tcamEntry(new row(dontcare(width)),new rowMetadata(shift = width,signatureLength = width)))
          println("############# TCAM entries #############\n" + this.toString())

    }

    override def toString(): String ={
      var ret =""
      for(i <-0 until((width+1))){
        for(entry<-tcam){
          if(entry.metadata.shift.equals(i)) {
            val entryData = entry.row.data.replace(DONT_CARE, "?")
            ret = ret+entryData+"\n"
          }
        }
      }
      ret
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
