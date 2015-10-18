package jadeutils.common

import org.slf4j.LoggerFactory
import org.slf4j.Logger

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class ExampleTest extends FunSuite with Logging {

	class Aaa(a: String, b: Double, c: Int, d: Long)
	{
		override def toString = ("""Aaa: {a: "%s", b: %f, c: %d, d: %d}""").format(
			a, b, c, d)
	}
	val a = new Aaa("a", 1.1, 2, 333)
	val b = new Aaa("b", 2.1, 3, 555)

	test("Test-scalatest") {
		logTrace("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc", 3.33, null)
		logTrace("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc")
		logTrace("{}, {}, {}, {}, {}, {}")

		logDebug("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc", 3.33, null)
		logDebug("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc")
		logDebug("{}, {}, {}, {}, {}, {}")

		logInfo("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc", 3.33, null)
		logInfo("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc")
		logInfo("{}, {}, {}, {}, {}, {}")

		logWarn("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc", 3.33, null)
		logWarn("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc")
		logWarn("{}, {}, {}, {}, {}, {}")

		logError("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc", 3.33, null)
		logError("{}, {}, {}, {}, {}, {}", a, b, 1, "ccc")
		logError("{}, {}, {}, {}, {}, {}")
	}

}



@RunWith(classOf[JUnitRunner])
class CommonTest extends FunSuite {

	import org.apache.commons.lang.StringUtils.isBlank
	import net.iharder.Base64

	test("Test-isBlank") {
		assert(isBlank(null))
		assert(isBlank(""))
		assert(isBlank("       "))
		assert(!isBlank("aaaaa"))
	}

	test("Test-Equals-ignore-blank") {
		assert( StrUtils.equalsIgnoreBlank("a","a"))
		assert(!StrUtils.equalsIgnoreBlank("a","b"))
		assert(!StrUtils.equalsIgnoreBlank("","a"))
		assert(!StrUtils.equalsIgnoreBlank("a",""))
		assert( StrUtils.equalsIgnoreBlank("",null))
		assert(!StrUtils.equalsIgnoreBlank("a",null))
	}

	test("Test-base64") {
		assert("rO0ABXQAC2hlbGxvIHdvcmxk" == Base64.encodeObject("hello world"))
	}
	
	/**
		* test except exception
		*/
	test("Test-Exception") {
		val s = "hi"
		intercept[IndexOutOfBoundsException] {
			s.charAt(-1)
		}
	}
	
	test("Test-Logging") {
		object LoggerTester extends Logging {
			def logDebugHello() {
				logDebug("hello {}, {}, {}, {}", 1, 2, 3, 4)
			}
		}
		LoggerTester.logDebugHello()
	}
	
	test("Test-env-Props") {

		trait MailComponent extends Logging {
			this: TestComponent =>

			trait MailSender {
				val host: String
				val port: String

				def sendMail() {
					logDebug("Sending mail: {}:{}", host, port)	
				}
			}
		}

		trait TestComponent extends EnvPropsComponent with MailComponent {

			object AdMailSender extends MailSender {
				val host = getProperty("email.user.host")
				val port = getProperty("email.user.port")
			}

		}

		object TestApp extends TestComponent {
			import java.util.Properties
			val envProps: Properties = new Properties()
			envProps.load(Thread.currentThread().getContextClassLoader(
        ).getResourceAsStream("mail.properties"))
		}

		TestApp.AdMailSender.sendMail()
	}

}

@RunWith(classOf[JUnitRunner])
class JsonTest extends FunSuite {

}

object JsonTest { 
	lazy val logger = LoggerFactory.getLogger(this.getClass)

	def getLoggerByName(name: String) = LoggerFactory.getLogger(name)
}

@RunWith(classOf[JUnitRunner])
class UtilsTest extends FunSuite {
	lazy val logger = UtilsTest.logger

	test("test-format-js") {
		val hu = new HttpBeautifyUtils
		val res = hu.formatJs("""if (a >0) { "<a href=\"http://www.google.com\">google</a>" } else { "<input type=\"text\" />" }""")
		assert(
		"""|if (a > 0) {
			 |    "<a href=\"http://www.google.com\">google</a>"
			 |} else {
			 |    "<input type=\"text\" />"
			 |}""".stripMargin
			== res.toString)
	}

}

object UtilsTest { 
	lazy val logger = LoggerFactory.getLogger(this.getClass)

	def getLoggerByName(name: String) = LoggerFactory.getLogger(name)
}
