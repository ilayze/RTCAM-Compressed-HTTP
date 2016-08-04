import org.scalatest.FunSuite
import src.main.com.jce.Gzip

/**
  * Created by izeidman on 8/4/2016.
  */
class gzipTests extends FunSuite{
  test("gzip") {
    val packet = "aaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbbbbaaaaaaaaaaaaaaaaaaaaaaaaaa"
    val input = Gzip.compress(packet.getBytes("UTF-8"))
    val decompressed = Gzip.decompress(input)
    assert(decompressed.get.equals(packet))
  }
}
