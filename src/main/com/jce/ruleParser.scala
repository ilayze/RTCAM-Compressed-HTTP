package src.main.com.jce

/**
  * Created by izeidman on 7/13/2016.
  */
class ruleParser {
    def parseRules(): Unit ={
    //  val lines = scala.io.Source.fromFile("classpath:community-rules").mkString
        val text = io.Source.fromInputStream(getClass.getResourceAsStream("/community-rules")).getLines()
        var lineNumber = 1
        for(line <- text)
          {
            println("Line number "+lineNumber+": "+line)
            lineNumber+=1
          }
      println(text)

    }

}
