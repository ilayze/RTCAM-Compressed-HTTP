import java.io.File
def recursiveListFiles(f: File): Array[File] = {
  val these = f.listFiles
  these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
}
val fileList = recursiveListFiles(new File("C:/Development/RTCAM-Compressed-HTTP/src/test/resources/realData/www.ynet.co.il/"));
fileList.foreach((file) => {
  if (file.isDirectory()) {
   // println(file.getAbsolutePath + " is directory")
  }
  else {
    println("Processing file: " + file.getName)
  }
})



