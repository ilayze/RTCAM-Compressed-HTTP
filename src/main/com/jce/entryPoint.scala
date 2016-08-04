package src.main.com.jce

object entryPoint {
  def main(args: Array[String]): Unit = {
    val parser = new ruleParser()
    val signatures = parser.parseRules()
    println(signatures)

  }
}