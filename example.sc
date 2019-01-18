import jadeutils.common.StrUtils

object example {
	println("Welcome to the Scala worksheet") //> Welcome to the Scala worksheet
	val aa = java.util.Base64.getEncoder.encode("hello扫地 world".getBytes())
                                                  //> aa  : Array[Byte] = Array(97, 71, 86, 115, 98, 71, 47, 109, 105, 97, 118, 10
                                                  //| 8, 110, 76, 65, 103, 100, 50, 57, 121, 98, 71, 81, 61)
	val str = new String(aa)                  //> str  : String = aGVsbG/miavlnLAgd29ybGQ=
	var ab = java.util.Base64.getDecoder.decode(str.getBytes())
                                                  //> ab  : Array[Byte] = Array(104, 101, 108, 108, 111, -26, -119, -85, -27, -100
                                                  //| , -80, 32, 119, 111, 114, 108, 100)
	val str2 = new String(ab)                 //> str2  : String = hello扫地 world

	val str3 = StrUtils.encodeBase64("hello扫地 world")
                                                  //> str3  : String = aGVsbG/miavlnLAgd29ybGQ=
	val str5 = StrUtils.decodeBase64(str)     //> str5  : String = hello扫地 world

}