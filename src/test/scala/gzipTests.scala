import com.jcraft.jzlib.Converter
import org.scalatest.FunSuite
import src.main.com.jce.gzipPacket

/**
  * Created by izeidman on 8/4/2016.
  */
class gzipTests extends FunSuite {
  test("packet length") {
    val gzipPacket = new gzipPacket("C104;C101;C108;C108;C111;C44;C32;C104;L4;D7;C33;C0;") //"hello, hello! " L4;D7; = 7 steps backward take 4 characters
    assert(gzipPacket.length.equals(14))
  }

  test("get function") {
    val gzipPacket = new gzipPacket("C104;C101;C108;C108;C111;C44;C32;C104;L4;D7;C33;C0;") //"hello, hello! " L4;D7; = 7 steps backward take 4 characters
    val partialPacket = gzipPacket.get(0, 4)
    assert(partialPacket.data.length.equals(5))
    assert(partialPacket.data.equals("hello"))
  }

  test("is pointer") {
    val gzipPacket = new gzipPacket("C104;C101;C108;C108;C111;C44;C32;C104;L4;D7;C33;C0;") //"hello, hello! " L4;D7; = 7 steps backward take 4 characters
    val notPointer = gzipPacket.isPointer(0, 7)
    assert(notPointer.equals(false))

    val lPointer = gzipPacket.isPointer(7, 8)
    assert(lPointer.equals(true))

    val dPointer = gzipPacket.isPointer(9, 11)
    assert(dPointer.equals(true))
  }

  test("get pointer - left bountary") {
    val gzipPacket = new gzipPacket("C97;C98;C99;L3;D3;") //abc[3,3]
    val subPacket = gzipPacket.get(0, 4)
    assert(subPacket.data.length.equals(5))
    assert(subPacket.data.equals("abcab"))
    assert(subPacket.pointerMetadata.isPointer.equals(true))
    assert(subPacket.pointerMetadata.currentPos.equals(2))
    assert(subPacket.pointerMetadata.length.equals(3))
  }

  test("get with full pointer") {
    val gzipPacket = new gzipPacket("C97;C98;C99;L3;D3;C97;C97;") //abc[3,3]aa
    val subPacket = gzipPacket.get(2, 6)
    assert(subPacket.data.length.equals(5))
    assert(subPacket.data.equals("cabca"))
    assert(subPacket.pointerMetadata.isPointer.equals(false))
  }

  test("pointer within pointer"){
    val gzipPacket = new gzipPacket("C97;C98;C99;L3;D3;L6;D6;") //abc[3,3][6,6]
    val check = gzipPacket.get(0,11)
    assert(check.data.equals("abcabcabcabc"))

  }

  test("three pointers"){
    val gzipPacket = new gzipPacket("C97;C97;C97;L3;D3;C97;C97;C97;L3;D3;C97;C97;C97;L3;D15;")
    val check = gzipPacket.get(0,17)
    println(check.data)
    assert(check.data.equals("aaaaaaaaaaaaaaaaaa"))


  }

  test("edge case from ynet"){
    val line = "<!-- Added by HTTrack --><meta http-equiv=\"content-type\" content=\"text/html;charset=UTF-8\" /><!-- /Added by HTTrack -->"
    val gzipAsciiCompressed = Converter.ToGzipAscii(line)
    println(String.format("Line: %s, GzipAcsii: %s",line,gzipAsciiCompressed))

    val gzipPacketCompressed = new gzipPacket(gzipAsciiCompressed)
  }
}
