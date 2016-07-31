/**
  * Created by izeidman on 7/28/2016.
  */

import org.scalatest.FunSuite
import src.main.com.jce.ruleParser

class SetSuite extends FunSuite {

  test("rules file with comments") {
    val parser = new ruleParser()
    parser.parseRules("/rules/comments")

  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}

