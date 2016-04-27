package jadeutils.web

import jadeutils.common.Logging

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

import java.net.URI
import java.util.Properties

@RunWith(classOf[JUnitRunner])
class UrlTest extends FunSuite with Logging {

	val p1 = new RequestPattern("/${username}/${userid}/${nickname}")
	val u1 = "/jack/233/skinner"

	val p2 = new RequestPattern("/${username}/${userid}/${nickname}.html")
	val u2 = "/jack/233/skinner.html"

	val p3 = new RequestPattern("/aaa/${username}/bbb/${userid}/ccc/${nickname}.html")
	val u3 = "/aaa/jack/bbb/233/ccc/skinner.html"

	val p4 = new RequestPattern("/aaa/bbb/ccc.html")
	val u4 = "/aaa/bbb/ccc.html"

	test("Test-Path-Pattern-params") {
		assert(p1.params == "${username}" :: "${userid}" :: "${nickname}" :: Nil)
		assert(p2.params == "${username}" :: "${userid}" :: "${nickname}" :: Nil)
		assert(p3.params == "${username}" :: "${userid}" :: "${nickname}" :: Nil)
		assert(p4.params == Nil)
	}

	test("Test-Path-Pattern-keys") {
		assert(p1.keys == "username" :: "userid" :: "nickname" :: Nil)
		assert(p2.keys == "username" :: "userid" :: "nickname" :: Nil)
		assert(p3.keys == "username" :: "userid" :: "nickname" :: Nil)
		assert(p4.keys == Nil)
	}

	test("Test-Path-Pattern-valuePtn") {
		assert(p1.valuePtn.toString == "/([^/]*)/([^/]*)/([^/]*)")
		assert(p2.valuePtn.toString == "/([^/]*)/([^/]*)/([^/]*).html")
		assert(p3.valuePtn.toString == "/aaa/([^/]*)/bbb/([^/]*)/ccc/([^/]*).html")
		assert(p4.valuePtn.toString == "/aaa/bbb/ccc.html")
	}

	test("Test-Path-Pattern-values") {
		assert(p1.matchPath(Method.ANY, u1) == 
			(true, Map("username" -> "jack", "userid" -> "233", 
				"nickname" -> "skinner")))
		assert(p2.matchPath(Method.ANY, u2) == 
			(true, Map("username" -> "jack", "userid" -> "233", 
				"nickname" -> "skinner")))
		assert(p3.matchPath(Method.ANY, u3) == 
			(true, Map("username" -> "jack", "userid" -> "233", 
				"nickname" -> "skinner")))
		assert(p4.matchPath(Method.ANY, u4) == (true, Map()))
	}
 
}



@RunWith(classOf[JUnitRunner])
class DispatherServletTest extends FunSuite with Logging { 

	import jadeutils.web.mock.MockRequest
	import jadeutils.web.mock.MockResponse

	class Ctl1 extends BasicController {
		val html = """<html><head><title>%s</title></head><body><h1>Hello! This is %s</h1>%s<br/>%s<br/></body></html>"""
		service("/${username}/${userid}/${nickname}") {
			(info) => {
				logger.info(html.format("logic 1", "logic 1", info.request.getRequestURI, 
				info.params.toString))
			}
		}

		service("/${username}/${userid}/${nickname}.html") {
			(info) => {
				logger.info(html.format("logic 2", "logic 2", info.request.getRequestURI, 
				info.params.toString))
			}
		}
	}

	class Ctl2 extends BasicController {
		val html = """<html><head><title>%s</title></head><body><h1>Hello! This is %s</h1>%s<br/>%s<br/></body></html>"""

		service("/aaa/${username}/bbb/${userid}/ccc/${nickname}.html") {
			(info) => {
				logger.info(html.format("logic 3", "logic 3", info.request.getRequestURI, 
				info.params.toString))
			}
		}

		service("/aaa/bbb/ccc.html") {
			(info) => {
				logger.info(html.format("logic 4", "logic 4", info.request.getRequestURI, 
				info.params.toString))
			}
		}
	}

	object MockDspth extends DispatherServlet {
		val controllers = new Ctl1 :: new Ctl2 :: Nil
	}

	test("Test-Dispath") {

		val resp = new MockResponse(System.out)

		MockDspth.doGet(new MockRequest("/jack/233/skinner"), resp)
		MockDspth.doGet(new MockRequest("/jack/233/skinner.html"), resp)
		MockDspth.doGet(new MockRequest("/aaa/jack/bbb/233/ccc/skinner.html"), resp)
		MockDspth.doGet(new MockRequest("/aaa/bbb/ccc.html"), resp)
		assert(1 == 1)
	}

}

