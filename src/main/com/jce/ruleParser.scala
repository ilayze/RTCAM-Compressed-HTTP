package src.main.com.jce

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by izeidman on 7/13/2016.
  */
class ruleParser {
  def parseRules(rules_file_path: String = "/community-rules"): ListBuffer[signature] = {

    val all_rules = io.Source.fromInputStream(getClass.getResourceAsStream(rules_file_path)).getLines()
    var lineNumber = 1
    var all_signatures: ListBuffer[signature] = new ListBuffer[signature]()
    for (snort_rule <- all_rules) {
      println("Line number " + lineNumber + ": " + snort_rule)
      lineNumber += 1

      if (!snort_rule.startsWith("#") && !snort_rule.startsWith("\n")) {
        val contents = snort_rule.split("content:")
        if (!(contents.length == 0)) {
          println(s"parsing rule: $snort_rule")
          for (i <- 0 until contents.length) {
            if (i == 0) {
              //skip
            }
            else {
              val cont = contents(i)
              val signature = cont.split(";")(0).stripPrefix("\"").stripSuffix("\"")
              println(s"new signature: $signature")
              all_signatures += new signature(signature, signature.getBytes.map("%02x".format(_)).mkString(""))
            }

          }

        }
      }
      else {
        println(s"skipping line: $snort_rule")
      }

    }
    println(all_rules)
    return all_signatures
  }
}
