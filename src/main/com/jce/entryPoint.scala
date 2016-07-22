package src.main.com.jce

object entryPoint {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    val parser = new ruleParser()
    parser.parseRules()

  }
}