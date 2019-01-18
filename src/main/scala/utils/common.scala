package jadeutils.common

import java.util.Random

import scala.xml.Text

import org.apache.commons.lang.StringUtils.isBlank

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
		* @return A base64 encoded String.
		*/
	def encodeBase64(data: Array[Byte]): String = //
		new String(java.util.Base64.getEncoder.encode(data))

	/**
		* Encodes a byte array into a bse64 String.
		*
		* @param str The byte arry to encode.
		* @return A base64 encoded String.
		*/
	def encodeBase64(str: String): String = encodeBase64(str.getBytes)

	/**
		* Decodes a base64 String.
		*
		* @param str a base64 encoded String to decode.
		* @return the decoded String.
		*/
	def decodeBase64(str: String) = //
		new String(java.util.Base64.getDecoder.decode(str.getBytes))
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

	private def matchLog(logFunc: (Seq[AnyRef]) => Unit, args: AnyRef*) { 
		args.toList match {
			case (h: TraversableOnce[_]) :: Nil => logFunc(h.toSeq.asInstanceOf[Seq[AnyRef]])
			case (h: Array[_]) :: Nil => logFunc(h.toSeq.asInstanceOf[Seq[AnyRef]])
			case _ => logFunc(args)
		}
	}

	def logTrace(msg: String, refs: Any*) {
		if (logger.isTraceEnabled) 
			matchLog((arrs) => { logger.trace(msg, arrs: _*) }, refs)
	}

	def logDebug(msg: String, refs: Any*) {
		if (logger.isDebugEnabled) 
			matchLog((arrs) => { logger.debug(msg, arrs: _*) }, refs)
	}

	def logInfo(msg: String, refs: Any*) {
		if (logger.isInfoEnabled) 
			matchLog((arrs) => { logger.info(msg, arrs: _*) }, refs)
	}

	def logWarn(msg: String, refs: Any*) {
		if (logger.isWarnEnabled) 
			matchLog((arrs) => { logger.warn(msg, arrs: _*) }, refs)
	}

	def logError(msg: String, refs: Any*) {
		if (logger.isErrorEnabled) 
			matchLog((arrs) => { logger.error(msg, arrs: _*) }, refs)
	}

}



object JsoupUtils {

	import scala.language.implicitConversions

	import org.jsoup.nodes.Element
	import org.jsoup.select.Elements

	implicit def jsoupElementsWrapper(elem: Element) = elem :: Nil

	implicit def jsoupElementsWrapper(elems: Elements) = for(i <- 0 until elems.size) yield elems.get(i)

}

object JavascriptUtils {
	import org.mozilla.javascript.Context
	import org.mozilla.javascript.Scriptable

	def enterContext = Context.enter

	def evaluateString(scope: Scriptable, scripts: String) = {
		val ctx = Context.enter
		val newScope = if (null == scope) ctx.initStandardObjects() else scope
		val res = ctx.evaluateString(newScope, scripts, null, 0, null)
		(newScope, res.toString)
	}

	def evaluateString(scripts: String): (Scriptable, String) = evaluateString(
		null, scripts) 
	
}

class HttpBeautifyUtils { }
object HttpBeautifyUtils extends Logging {
	import scala.io.Source
	import org.mozilla.javascript.Scriptable

	private[this] val jsBeautyScop: Scriptable = {
		val scripts = Source.fromInputStream(classOf[HttpBeautifyUtils]
			.getResourceAsStream("/js/beautifyjs/beautify.js")).mkString
		JavascriptUtils.evaluateString(scripts)._1
	}

	private[this] val cssBeautyScop: Scriptable = {
		val scripts = Source.fromInputStream(classOf[HttpBeautifyUtils]
			.getResourceAsStream("/js/beautifyjs/beautify-css.js")).mkString
		JavascriptUtils.evaluateString(scripts)._1
	}

	private[this] val htmlBeautyScop: Scriptable = {
		val scripts = Source.fromInputStream(classOf[HttpBeautifyUtils]
			.getResourceAsStream("/js/beautifyjs/beautify-html.js")).mkString
		JavascriptUtils.evaluateString(scripts)._1
	}

	def formatJs(str: String) = {
		val code = str.replaceAll("""\\""","""\\\\""").replaceAll(
			""""""", "\\\\\"") .replaceAll("\n", "\\\\n")
		logger.debug(code)
		val scripts = """js_beautify("%s")""" format code
		logger.debug(scripts)
		JavascriptUtils.evaluateString(jsBeautyScop, scripts)._2;
	}
}
