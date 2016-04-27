package jadeutils.web.mock

import java.io.PrintWriter

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

class MockRequest(requestURI: String) extends HttpServletRequest {
	import java.util.Vector


	override def getRequestURI = requestURI
	override def getContextPath = ""

	override def authenticate(x$1: javax.servlet.http.HttpServletResponse): Boolean = false
	override def changeSessionId(): String = null
	override def getAuthType(): String = null
	override def getCookies(): Array[javax.servlet.http.Cookie] = null
	override def getDateHeader(x$1: String): Long = 0

	override def getHeader(name: String): String = name match {
		case "Host" => "www.jade-dungeon.net"
		case "User-Agent" => "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0"
		case "Accept" => "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
		case _ => ""
	}

	override def getHeaderNames(): java.util.Enumeration[String] = {
		val cc = new Vector[String]
		cc add "Host"
		cc add "User-Agent"
		cc add "Accept"
		cc.elements
	}

	override def getHeaders(name: String): java.util.Enumeration[String] =  name match {
		case "Host" => {
			val cc = new Vector[String]
			cc add "www.jade-dungeon.net"
			cc.elements
		}
		case "User-Agent" =>  {
			val cc = new Vector[String]
			cc add "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:40.0) Gecko/20100101 Firefox/40.0"
			cc.elements
		}
		case "Accept" =>  {
			val cc = new Vector[String]
			cc add "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
			cc.elements
		}
		case _ => (new Vector[String]).elements
	}

	override def getIntHeader(x$1: String): Int = 0
	override def getMethod(): String = null
	override def getPart(x$1: String): javax.servlet.http.Part = null
	override def getParts(): java.util.Collection[javax.servlet.http.Part] = null
	override def getPathInfo(): String = null
	override def getPathTranslated(): String = null
	override def getQueryString(): String = null
	override def getRemoteUser(): String = null
	override def getRequestURL(): StringBuffer = null
	override def getRequestedSessionId(): String = null
	override def getServletPath(): String = null
	override def getSession(): javax.servlet.http.HttpSession = null
	override def getSession(x$1: Boolean): javax.servlet.http.HttpSession = null
	override def getUserPrincipal(): java.security.Principal = null
	override def isRequestedSessionIdFromCookie(): Boolean = false
	override def isRequestedSessionIdFromURL(): Boolean = false
	override def isRequestedSessionIdFromUrl(): Boolean = false
	override def isRequestedSessionIdValid(): Boolean = false
	override def isUserInRole(x$1: String): Boolean = false
	override def login(x$1: String,x$2: String): Unit = {}
	override def logout(): Unit = {}
	override def upgrade[HttpUpgradeHandler <: javax.servlet.http.HttpUpgradeHandler](x$1: Class[HttpUpgradeHandler]): HttpUpgradeHandler = null.asInstanceOf[HttpUpgradeHandler]
	override def getAsyncContext(): javax.servlet.AsyncContext = null
	override def getAttribute(x$1: String): Object = null
	override def getAttributeNames(): java.util.Enumeration[String] = null
	override def getCharacterEncoding(): String = null
	override def getContentLength(): Int = 0
	override def getContentLengthLong(): Long = 0
	override def getContentType(): String = null
	override def getDispatcherType(): javax.servlet.DispatcherType = null
	override def getInputStream(): javax.servlet.ServletInputStream = null
	override def getLocalAddr(): String = null
	override def getLocalName(): String = null
	override def getLocalPort(): Int = 0
	override def getLocale(): java.util.Locale = null
	override def getLocales(): java.util.Enumeration[java.util.Locale] = null
	override def getParameter(x$1: String): String = null
	override def getParameterMap(): java.util.Map[String,Array[String]] = null
	override def getParameterNames(): java.util.Enumeration[String] = null
	override def getParameterValues(x$1: String): Array[String] = null
	override def getProtocol(): String = null
	override def getReader(): java.io.BufferedReader = null
	override def getRealPath(x$1: String): String = null
	override def getRemoteAddr(): String = null
	override def getRemoteHost(): String = null
	override def getRemotePort(): Int = 0
	override def getRequestDispatcher(x$1: String): javax.servlet.RequestDispatcher = null
	override def getScheme(): String = null
	override def getServerName(): String = null
	override def getServerPort(): Int = 0
	override def getServletContext(): javax.servlet.ServletContext = null
	override def isAsyncStarted(): Boolean = false
	override def isAsyncSupported(): Boolean = false
	override def isSecure(): Boolean = false
	override def removeAttribute(x$1: String): Unit = {}
	override def setAttribute(x$1: String,x$2: Any): Unit = {}
	override def setCharacterEncoding(x$1: String): Unit = {}
	override def startAsync(x$1: javax.servlet.ServletRequest,x$2: javax.servlet.ServletResponse): javax.servlet.AsyncContext = null
	override def startAsync(): javax.servlet.AsyncContext = null
}

class MockResponse(out: java.io.OutputStream) extends HttpServletResponse {
	val writer = new PrintWriter(out)
	override def getWriter = writer

	override  def addCookie(x$1: javax.servlet.http.Cookie): Unit = {}
	override  def addDateHeader(x$1: String,x$2: Long): Unit = {}
	override  def addHeader(x$1: String,x$2: String): Unit = {}
	override  def addIntHeader(x$1: String,x$2: Int): Unit = {}
	override  def containsHeader(x$1: String): Boolean = false
	override  def encodeRedirectURL(x$1: String): String = null
	override  def encodeRedirectUrl(x$1: String): String = null
	override  def encodeURL(x$1: String): String = null
	override  def encodeUrl(x$1: String): String = null
	override  def getHeader(x$1: String): String = null
	override  def getHeaderNames(): java.util.Collection[String] = null
	override  def getHeaders(x$1: String): java.util.Collection[String] = null
	override  def getStatus(): Int = 0
	override  def sendError(x$1: Int): Unit = {}
	override  def sendError(x$1: Int,x$2: String): Unit = {}
	override def sendRedirect(x$1: String): Unit = {}
	override def setDateHeader(x$1: String,x$2: Long): Unit = {}
	override def setHeader(x$1: String,x$2: String): Unit = {}
	override def setIntHeader(x$1: String,x$2: Int): Unit = {}
	override def setStatus(x$1: Int,x$2: String): Unit = {}
	override def setStatus(x$1: Int): Unit = {}
	override def flushBuffer(): Unit = {}
	override def getBufferSize(): Int = 0
	override def getCharacterEncoding(): String = null
	override def getContentType(): String = null
	override def getLocale(): java.util.Locale = null
	override def getOutputStream(): javax.servlet.ServletOutputStream = null
	override def isCommitted(): Boolean = false
	override def reset(): Unit = {}
	override def resetBuffer(): Unit = {}
	override def setBufferSize(x$1: Int): Unit = {}
	override def setCharacterEncoding(x$1: String): Unit = {}
	override def setContentLength(x$1: Int): Unit = {}
	override def setContentLengthLong(x$1: Long): Unit = {}
	override def setContentType(x$1: String): Unit = {}
	override def setLocale(x$1: java.util.Locale): Unit = {}
}

