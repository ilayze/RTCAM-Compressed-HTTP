package src.main.com.jce

/**
  * Created by izeidman on 8/4/2016.
  */
class gzipPacket(val data:String) {

  val dataSplitted: Array[String] = data.split(";")

  def get(begin: Int, end: Int) : String = {
      val dataSplittedSlice = dataSplitted.slice(begin,end+1)
      var ret = ""
      for (i <- dataSplittedSlice){
        if(i.startsWith("C"))
          {
            val asciiChar = i.substring(1)
            ret+=asciiChar.toInt.toChar
          }
        else{
          throw new Exception("Pointer")
        }
      }
      ret
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
              length+= i.substring(1,i.length()).toInt
            }
          }
        return length
      }

}

class pointerMetadata(val length:Int,val currentPos:Int)
