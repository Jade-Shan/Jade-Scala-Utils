package jadeutils.common

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

import net.iharder.Base64
import org.apache.commons.lang.StringUtils.isBlank



@RunWith(classOf[JUnitRunner])
class CommonTest extends FunSuite {

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
				logger.debug("hello {}, {}, {}, {}", 1.asInstanceOf[AnyRef], 
					2.asInstanceOf[AnyRef], 3.asInstanceOf[AnyRef], 4.asInstanceOf[AnyRef])
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
					logger.debug("Sending mail: {}:{}", host.asInstanceOf[AnyRef], 
						port.asInstanceOf[AnyRef])	
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
