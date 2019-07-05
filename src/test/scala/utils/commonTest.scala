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
		assert("aGVsbG8gd29ybGQ=" == 
			java.util.Base64.getEncoder.encodeToString("hello world".getBytes()))
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

	test("test-run-js") {
		import scala.io.Source
		val scripts = Source.fromFile("test.js").mkString
		logger.info(scripts)
		val result = JavascriptUtils.evaluateString(scripts + "; d[0];")
		logger.info(result._2)
	}

	test("test-run-js2") {
		import scala.io.Source
		val scripts = Source.fromFile("test2.js").mkString
		logger.info(scripts)
		val result = JavascriptUtils.evaluateString(scripts + "; d[0];")
		logger.info(result._2)
	}

	test("test-format-js") {
		val res = HttpBeautifyUtils.formatJs("""if (a >0) { "<a href=\"http://www.google.com\">google</a>" } else { "<input type=\"text\" />" }""")
		assert(
			"""|if (a > 0) {
				|    "<a href=\"http://www.google.com\">google</a>"
				|} else {
				|    "<input type=\"text\" />"
				|}""".stripMargin == res)
	}

}

object UtilsTest { 
	lazy val logger = LoggerFactory.getLogger(this.getClass)

	def getLoggerByName(name: String) = LoggerFactory.getLogger(name)
}

/* 
@RunWith(classOf[JUnitRunner])
class OpenCVTest extends FunSuite with Logging {

	test("test-opencv3") {

		trait OpencvComponent extends Logging {
			this: TestComponent =>

			trait OpencvCfg {
				val sharelibBasedir: String
				val sharelibPostfix: String

				def testInitOpencv() {
					import org.opencv.core.Core
					import org.opencv.core.CvType
					import org.opencv.core.Mat
					import org.opencv.core.Scalar

					// System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
					// System.load("/usr/local/share/OpenCV/java/libopencv_java310.so")
					System.load("%s%s%s".format(sharelibBasedir, Core.NATIVE_LIBRARY_NAME, sharelibPostfix))
					logDebug("Welcome to OpenCV " + Core.VERSION)
					val m = new Mat(5, 10, CvType.CV_8UC1, new Scalar(0))
					logDebug("OpenCV Mat: " + m)
					val mr1 = m.row(1)
					mr1.setTo(new Scalar(1))
					val mc5 = m.col(5)
					mc5.setTo(new Scalar(5))
					logInfo("OpenCV Mat data:\n" + m.dump())
				}
			}
		}

		trait TestComponent extends EnvPropsComponent with OpencvComponent {

			object DefaultOpencvCfg extends OpencvCfg {
				val sharelibBasedir = getProperty("opencv.sharelib.basedir")
				val sharelibPostfix = getProperty("opencv.sharelib.postfix")
			}

		}

		object TestApp extends TestComponent {
			import java.util.Properties
			val envProps: Properties = new Properties()
			envProps.load(Thread.currentThread().getContextClassLoader().getResourceAsStream(
				"opencv.properties"))
		}

		TestApp.DefaultOpencvCfg.testInitOpencv()

	}

}
*/
