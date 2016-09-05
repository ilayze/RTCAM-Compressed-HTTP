import org.scalatest.FunSuite
import src.main.com.jce.{rowMetadata, row, tcamEntry, tcamSimulator}
/**
  * Created by izeidman on 8/4/2016.
  */

class tcamTests extends FunSuite{
  test("initialize tcam - add don't cares to left"){
    val tcamSimulator = new tcamSimulator(width = 4)
    tcamSimulator.initialize("abcd")
    val entry = tcamSimulator.lookUp(",abc")

    assert(entry.shift.equals(1))
  }

  test("initialize signature less than width - should have don't cares on right"){
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("abcd")
    val entry = tcamSimulator.lookUp("eabcd")

    assert(entry.shift.equals(1))
  }

  test("add same entry twice, should only add it once"){
    val tcamSimulator = new tcamSimulator(width = 4)
    val tcamEntry = new tcamEntry(new row(data = "abcd"),new rowMetadata(shift = 0,signatureLength = 4,signatureIndex = -1, signatureNumber = -1))
    tcamSimulator.addEntry(tcamEntry)
    tcamSimulator.addEntry(tcamEntry)

    assert(tcamSimulator.tcam.length == 1)
  }

  test("pattern less than width"){
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initialize("abc")
    val entry = tcamSimulator.lookUp("abccc")

    assert(entry.shift.equals(0))

  }

  test("Initialize with rule parser"){
    val tcamSimulator = new tcamSimulator(width = 5)
    tcamSimulator.initializeWithParser("/rules/signatures1")
    val entry1 = tcamSimulator.lookUp("abcd")
    assert(entry1.shift.equals(0))

    val entry2 = tcamSimulator.lookUp("efgh")
    assert(entry2.shift.equals(0))

  }
}
