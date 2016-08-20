package src.main.com.jce

/**
  * Created by izeidman on 8/4/2016.
  */
class gzipPacket(val data:String) {

  val dataSplitted: Array[String] = data.split(";")

  def get(begin: Int, end: Int) : Array[String] = {
      val dataSplittedSlice = dataSplitted.slice(begin,end+1)
      dataSplittedSlice
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
        return dataSplitted.length
      }

}
