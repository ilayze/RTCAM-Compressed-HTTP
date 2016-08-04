import org.scalatest.FunSuite
import src.main.com.jce.tcamSimulator
/**
  * Created by izeidman on 8/4/2016.
  */
class tcamTests extends FunSuite{
  test("initialize tcam"){
    val tcamSimulator = new tcamSimulator(width = 4)
  }
}
