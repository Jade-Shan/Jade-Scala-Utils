package net.jadedungeon.scalautil.net.http

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

import net.jadedungeon.scalautil.common.Logging

import java.net.URI
import java.util.Properties

@RunWith(classOf[JUnitRunner])
class DispatherServletTest extends FunSuite with Logging { 

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
