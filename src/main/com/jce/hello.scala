package src.main.com.jce

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")
    var parser = new ruleParser()
    parser.parseRules()

  }
}