import algorithm.rtcamCompressedHttp
import org.scalatest.FunSuite
import src.main.com.jce.{gzipPacket, tcamSimulator}

/**
  * Created by izeidman on 8/15/2016.
  */
class algorithmTests extends FunSuite {
  test("Execute algorithm") {
    val tcamSimulator = new tcamSimulator(width = 4)
    val gzipPacket = new gzipPacket("C104;C101;C108;C108;C111;C44;C32;C104;L4;D7;C33;C0;")
    val rtcamCompressedHttp = new rtcamCompressedHttp(packet = gzipPacket, tcam = tcamSimulator)
    rtcamCompressedHttp.execute()
  }
}

