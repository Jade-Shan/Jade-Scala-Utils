package jadeutils.common

import java.util.Random

import scala.xml.Text

import org.apache.commons.lang.StringUtils.isBlank

import net.iharder.Base64

import org.slf4j.LoggerFactory
import org.slf4j.Logger

object ObjUtils {

	def hashField(field: Any) = if (null == field) 0 else field.hashCode

}

object StrUtils {

	def isCharBlank(char: Char) = {
		char == ' ' || char == '\t' || char == '\n' || char == '\r'
	}

	def equalsIgnoreBlank(a: String, b: String): Boolean = {
		a == b || (isBlank(a) && isBlank(b))
	}

	/* rand star tools */
	var randGen: Random = new Random();
	val numAndChar = "0123456789abcdefghijklmnopqrstuvwxyz" +
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	val numAndCharMaxIdx = numAndChar.length - 1 
	def randomNumLetterStr(len : Int): String = {
		if (len < 1) null;
		else {
			val buff = new Array[Char](len)
			for (i <- 0 until buff.length)
				buff(i) = numAndChar(randGen.nextInt(numAndCharMaxIdx))
			new String(buff);
		}
	}

	/**
		* Encodes a byte array into a bse64 String.
		*
		* @param data The byte arry to encode.
		* @param offset the offset of the bytearray to begin encoding at.
		* @param len the length of bytes to encode.
		* @param lineBreaks True if the encoding should contain line breaks and 
		*	       false if it should not.
		* @return A base64 encoded String.
		*/
	def encodeBase64(data: Array[Byte], offset: Int, len: Int, 
		lineBreaks: Boolean): String = {
		Base64.encodeBytes(data, offset, len, 
			if (lineBreaks) Base64.NO_OPTIONS else Base64.DONT_BREAK_LINES)
		}

	/**
		* Encodes a byte array into a bse64 String.
		*
		* @param data The byte arry to encode.
		* @param lineBreaks True if the encoding should contain line breaks and 
		*        false if it should not.
		* @return A base64 encoded String.
		*/
	def encodeBase64(data: Array[Byte], lineBreaks: Boolean):String = 
	encodeBase64(data, 0, data.length, lineBreaks)

	/**
		* Encodes a byte array into a base64 String.
		*
		* @param data a byte array to encode.
		* @return a base64 encode String.
		*/
	def encodeBase64(data: Array[Byte]):String = encodeBase64(data, false)


	/**
		* Decodes a base64 String.
		* Unlike Base64.decode() this method does not try to detect and decompress 
		* a gzip-compressed input.
		*
		* @param data a base64 encoded String to decode.
		* @return the decoded String.
		*/
	def decodeBase64(data: String) = {
		var bytes: Array[Byte] = null
		try {
			bytes = data.getBytes("UTF-8");
		} catch {
			case e: java.io.UnsupportedEncodingException =>
			bytes = data.getBytes();
		}
		Base64.decode(bytes, 0, bytes.length, Base64.NO_OPTIONS);
	}
}

object IOUtils {
	import java.io.ByteArrayInputStream
	import java.io.ByteArrayOutputStream
	import java.io.IOException
	import java.io.InputStream
	import java.io.UnsupportedEncodingException
	import java.util.zip.GZIPInputStream

	import scala.collection.mutable.ArrayBuffer

	def readStream(is: InputStream, bufferSize: Int) = {
		var result = new ArrayBuffer[Byte]
		var buffer = new Array[Byte](bufferSize)
		var count = is.read(buffer)
		while (count > 0) {
			result.appendAll(buffer)
			count = is.read(buffer)
		}
		result.toArray
	}

	def unZipGZ(data: Array[Byte]) = {
		var result:Array[Byte] = null

		val out	= new ByteArrayOutputStream()
		val in = new ByteArrayInputStream(data)
		val zip = new GZIPInputStream(in)

		var buffer = new Array[Byte](1024)
		var count = zip.read(buffer, 0, buffer.length)
		while(count > 0) {
			out.write(buffer, 0, count)
			count = zip.read(buffer, 0, buffer.length)
		}
		result = out.toByteArray()
		out.flush()
		out.close()
		zip.close()
		in.close()
		result
	}

}



object XMLUtils {

	def newTextAttr(v: Any) = if (null == v) null else Text(v.toString)

}

trait Logging {

	lazy val logger = LoggerFactory.getLogger(this.getClass)

	def getLoggerByName(name: String) = LoggerFactory.getLogger(name)

}



object JsoupUtils {

	import scala.language.implicitConversions

	import org.jsoup.nodes.Element
	import org.jsoup.select.Elements

	implicit def jsoupElementsWrapper(elem: Element) = elem :: Nil

	implicit def jsoupElementsWrapper(elems: Elements) = for(i <- 0 until elems.size) yield elems.get(i)

}



class HttpBeautifyUtils {
	import scala.io.Source
	import org.mozilla.javascript.Context
	import org.mozilla.javascript.Scriptable

	val ctx = Context.enter

	val jsBeautyScop: Scriptable = {
		val scope = ctx.initStandardObjects()
		val scripts = Source.fromInputStream(classOf[HttpBeautifyUtils]
			.getResourceAsStream("/js/beautifyjs/beautify.js")).mkString
		ctx.evaluateString(scope, scripts, null, 0, null)
		scope
	}

	val cssBeautyScop: Scriptable = {
		val scope = ctx.initStandardObjects()
		val scripts = Source.fromInputStream(classOf[HttpBeautifyUtils]
			.getResourceAsStream("/js/beautifyjs/beautify-css.js")).mkString
		ctx.evaluateString(scope, scripts, null, 0, null)
		scope
	}

	val htmlBeautyScop: Scriptable = {
		val scope = ctx.initStandardObjects()
		val scripts = Source.fromInputStream(classOf[HttpBeautifyUtils]
			.getResourceAsStream("/js/beautifyjs/beautify-html.js")).mkString
		ctx.evaluateString(scope, scripts, null, 0, null)
		scope
	}

	def formatJs(str: String) = {
		val code = str.replaceAll("""\\""","""\\\\""").replaceAll(""""""", """\\"""")
			.replaceAll("\n", "\\\\n")
		println(code)
		println("""js_beautify("%s")""" format code)
		ctx.evaluateString(jsBeautyScop, """js_beautify("%s")""" format code, null, 0, null)
	}

}
