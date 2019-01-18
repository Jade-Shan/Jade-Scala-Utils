import jadeutils.common.StrUtils

object example {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(94); 
	println("Welcome to the Scala worksheet");$skip(73); 
	val aa = java.util.Base64.getEncoder.encode("hello扫地 world".getBytes());System.out.println("""aa  : Array[Byte] = """ + $show(aa ));$skip(26); 
	val str = new String(aa);System.out.println("""str  : String = """ + $show(str ));$skip(61); 
	var ab = java.util.Base64.getDecoder.decode(str.getBytes());System.out.println("""ab  : Array[Byte] = """ + $show(ab ));$skip(27); 
	val str2 = new String(ab);System.out.println("""str2  : String = """ + $show(str2 ));$skip(53); 

	val str3 = StrUtils.encodeBase64("hello扫地 world");System.out.println("""str3  : String = """ + $show(str3 ));$skip(39); 
	val str5 = StrUtils.decodeBase64(str);System.out.println("""str5  : String = """ + $show(str5 ))}

}
