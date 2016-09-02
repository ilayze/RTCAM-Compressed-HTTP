package src.main.com.jce

/**
  * Created by izeidman on 8/4/2016.
  */
class gzipPacket(val data:String) {

  val dataSplitted: Array[String] = data.split(";")

  def get(begin: Int, end: Int) : subPacket = {
      var subPacket = new subPacket(data = "",pointerMetadata = new notPointer())
      val dataSplittedSlice = dataSplitted.slice(begin,end+1)
      var subPacketData = ""
      var pos = 0
      while(subPacketData.length<end+1-begin){
        val nextChar = dataSplittedSlice(pos)
        if(nextChar.startsWith("C"))
        {
          val asciiChar = nextChar.substring(1)
          subPacketData+=asciiChar.toInt.toChar
          pos+=1
        }
        else if(nextChar.startsWith("L")) {
          var dChar:Int = 0
          val lChar = getPointerValue(nextChar)
          if(pos.equals(dataSplittedSlice.length-1))//bring the D
          {
             dChar = getPointerValue(dataSplitted(end+1))
          }
          else{
            dChar = getPointerValue(dataSplittedSlice(pos+1))
          }

          val charsLeft = end+1-begin-subPacketData.length()

          if(charsLeft<=getPointerValue(nextChar)){
            subPacketData+=get(begin+pos-dChar,begin+pos-dChar+charsLeft-1).data
            subPacket.pointerMetadata = new pointerMetadata(length=lChar,currentPos = charsLeft,isPointer= true)
          }
          else{
            subPacketData+=get(begin+pos-dChar,begin+pos-dChar+lChar-1).data
            pos+=2
          }

        }
      }
      subPacket.data = subPacketData
      return subPacket
  }

  def isPointer(begin: Int, end: Int): Boolean ={
    val dataSplittedSlice = dataSplitted.slice(begin,end+1)
    for (i <- dataSplittedSlice){
      if(i.contains("L") || i.contains("D"))
        return true
    }
    return false
  }

  def length: Int ={
        var length:Int=0
        for (i<-dataSplitted)
          {
            if(i.startsWith("C")){
              length+=1
            }
            else if(i.startsWith("L")){
              length+= getPointerValue(i)
            }
          }
        return length
      }

  def getPointerValue(i: String): Int = {
    i.substring(1, i.length()).toInt
  }
}

class pointerMetadata(val length:Int,val currentPos:Int,val isPointer:Boolean)

class notPointer extends pointerMetadata(length = -1,currentPos = -1, isPointer = false)

class subPacket(var data:String,var pointerMetadata: pointerMetadata)
