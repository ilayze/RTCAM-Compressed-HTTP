/**
  * Created by izeidman on 7/28/2016.
  */

import org.scalatest.FunSuite
import src.main.com.jce.ruleParser

class RuleParserSuite extends FunSuite {

  test("rules file with comments") {
    val parser = new ruleParser()
    parser.parseRules("/rules/comments")

  }

  test("rule with content"){
    val parser = new ruleParser()
    val signatures = parser.parseRules("/rules/content")
    println(signatures)
    assert(signatures.length == 1)
    val expected_signature_regular = "|09|sidisalim|05|myvnc|03|com|00|"
    assert(signatures(0).regular.equals(expected_signature_regular))
    val expected_signature_hex = "7c30397c7369646973616c696d7c30357c6d79766e637c30337c636f6d7c30307c"
    assert(signatures(0).hex.equals(expected_signature_hex))
  }

  test("Invoking head on an empty Set should produce NoSuchElementException") {
    intercept[NoSuchElementException] {
      Set.empty.head
    }
  }
}

