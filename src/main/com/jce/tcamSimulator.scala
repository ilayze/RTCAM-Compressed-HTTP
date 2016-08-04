package src.main.com.jce

import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 8/4/2016.
  *
  * Simulator for Ternary content-addressable memory(TCAM) see: https://en.wikipedia.org/wiki/Content-addressable_memory#Ternary_CAMs
  */
class tcamSimulator(val width:Int) {
    var tcam = new ListBuffer[tcamEntry]

    def lookUp(key: String): rowMetadata =
    {
        return null;
    }

    def addEntry(entry: tcamEntry): Unit =
    {
        tcam.append(entry)
    }
}

class rowMetadata(val shift:Int)

class row(val data:String)

class tcamEntry(val row:row, val metadata:rowMetadata)
