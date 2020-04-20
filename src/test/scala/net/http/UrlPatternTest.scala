package net.jadedungeon.scalautil.net.http

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

import net.jadedungeon.scalautil.common.Logging

import java.net.URI
import java.util.Properties

@RunWith(classOf[JUnitRunner])
class UrlPatternTest extends FunSuite with Logging {

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