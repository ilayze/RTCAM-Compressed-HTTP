import org.scalatest.FunSuite
import src.main.com.jce.{rowMetadata, row, tcamEntry, tcamSimulator}
/**
  * Created by izeidman on 8/4/2016.
  */
class tcamTests extends FunSuite{
  test("initialize tcam"){
    val tcamSimulator = new tcamSimulator(width = 4)
  }

  test("add same entry twice, should only add it once"){
    val tcamSimulator = new tcamSimulator(width = 4)
    val tcamEntry = new tcamEntry(new row(data = "abcd"),new rowMetadata(shift = 0,signatureLength = 4))
    tcamSimulator.addEntry(tcamEntry)
    tcamSimulator.addEntry(tcamEntry)

    assert(tcamSimulator.tcam.length == 1)
  }
}
