package src.main.com.jce

object entryPoint {
  def main(args: Array[String]): Unit = {
    println("Hello, from entry point!")
    val parser = new ruleParser()
    parser.parseRules()

  }
}