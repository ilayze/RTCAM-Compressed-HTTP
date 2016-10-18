import org.scalatest.FunSuite
import src.main.com.jce.{subSignatureMetadata, row, tcamEntry, tcamSimulator}

/**
  * Created by izeidman on 8/4/2016.
  */

class tcamTests extends FunSuite {
  test("initialize tcam - add don't cares to left") {
    val tcamSimulator = new tcamSimulator(width = 4)
    tcamSimulator.initialize("abcd")
    val entry = tcamSimulator.lookUp(",abc")

    assert(entry(0).shift.equals(1))
  }

  test("initialize signature less than width - should have don't cares on right") {
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("abcd")
    val entry = tcamSimulator.lookUp("eabcd")

    assert(entry(0).shift.equals(1))
  }

  test("add same entry twice, should only add it once") {
    val tcamSimulator = new tcamSimulator(width = 4)
    tcamSimulator.addEntry(new row(data = "abcd"), new subSignatureMetadata(shift = 0, signatureLength = 4, signatureIndex = -1, signatureNumber = -1))
    tcamSimulator.addEntry(new row(data = "abcd"), new subSignatureMetadata(shift = 0, signatureLength = 4, signatureIndex = -1, signatureNumber = -1))

    assert(tcamSimulator.tcam.length == 1)
  }

  test("pattern less than width") {
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("abc")
    val entry = tcamSimulator.lookUp("abccc")

    assert(entry(0).shift.equals(0))

  }

  test("Initialize with rule parser") {
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initializeWithParser("/rules/signatures1")
    val entry1 = tcamSimulator.lookUp("abcd")
    assert(entry1(0).shift.equals(0))

    val entry2 = tcamSimulator.lookUp("efgh")
    assert(entry2(0).shift.equals(0))

  }

  test("snort to hex regular string"){
    val tcamSimulator = new tcamSimulator(width = 5)
    val hexRepresentation = tcamSimulator.snortToHex("abcd")
    assert(hexRepresentation.equals("61626364"))
  }

  test("snort to hex ascii string"){
    val tcamSimulator = new tcamSimulator(width = 5)
    val hexRepresentation = tcamSimulator.snortToHex("|06|")
    assert(hexRepresentation.equals("06"))
  }

  test("snort to hex regular and ascii"){
    val tcamSimulator = new tcamSimulator(width = 5)
    val hexRepresentation = tcamSimulator.snortToHex("2|00 00 00 06 00 00 00|Drives|24 00|")
    assert(hexRepresentation.equals("32000000060000004472697665732400"))
  }
}
