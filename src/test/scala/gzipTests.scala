import org.scalatest.FunSuite
import src.main.com.jce.{gzipPacket}

/**
  * Created by izeidman on 8/4/2016.
  */
class gzipTests extends FunSuite{
  test("packet length") {
    val gzipPacket = new gzipPacket("C104;C101;C108;C108;C111;C44;C32;C104;L4;D7;C33;C0;") //"hello, hello! " L4;D7; = 7 steps backward take 4 characters
    assert(gzipPacket.length.equals(12))
  }

  test("get function"){
    val gzipPacket = new gzipPacket("C104;C101;C108;C108;C111;C44;C32;C104;L4;D7;C33;C0;") //"hello, hello! " L4;D7; = 7 steps backward take 4 characters
    val partialPacket = gzipPacket.get(0,4)
    assert(partialPacket.length.equals(5))
    assert(partialPacket(0).equals("C104"))
    assert(partialPacket(1).equals("C101"))
    assert(partialPacket(2).equals("C108"))
    assert(partialPacket(3).equals("C108"))
    assert(partialPacket(4).equals("C111"))
  }

  test("is pointer"){
    val gzipPacket = new gzipPacket("C104;C101;C108;C108;C111;C44;C32;C104;L4;D7;C33;C0;") //"hello, hello! " L4;D7; = 7 steps backward take 4 characters
    val notPointer = gzipPacket.isPointer(0,7)
    assert(notPointer.equals(false))

    val lPointer = gzipPacket.isPointer(7,8)
    assert(lPointer.equals(true))

    val dPointer = gzipPacket.isPointer(9,11)
    assert(dPointer.equals(true))
  }
}
