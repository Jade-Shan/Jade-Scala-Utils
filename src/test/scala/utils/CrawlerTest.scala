package jadeutils.crawler

import scala.collection.mutable.ListBuffer

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.message.BasicNameValuePair;

@RunWith(classOf[JUnitRunner])
class CrawlerTest extends FunSuite {

//	test("Test-Http") {
//		val request = new HttpPost(
//			"http://hotel.elong.com/wuhan/01801567/");
//		request.setHeader("Accept-Encoding", "gzip,deflate,sdch");
//		val params = new java.util.ArrayList[NameValuePair]()
//		params.add(new BasicNameValuePair("Referer", "http://hotel.elong.com"))
//		params.add(new BasicNameValuePair("isSquare", "false"))
//		params.add(new BasicNameValuePair("hotelId", "01801567"))
//		params.add(new BasicNameValuePair("requestInfo.CheckInDate", "2014-08-20"))
//		params.add(new BasicNameValuePair("requestInfo.CheckOutDate", "2014-08-21"))
//		request.setEntity(new UrlEncodedFormEntity(params))
//
//		val httpclient = HttpClients.createDefault()
//		val resp: HttpResponse	= httpclient.execute(request)
//		val statusCode = resp.getStatusLine().getStatusCode()
//
//		if (statusCode >= 300 && statusCode < 400) {
//			System.out.println("重定向："
//				+ resp.getFirstHeader("Location").getValue());
//		} else if (statusCode == 200) {
//			import jadeutils.common.IOUtils
//
//			var bytes = IOUtils.readStream(resp.getEntity().getContent(), 2048)
//			if (resp.getEntity().getContentEncoding() != null
//				&& resp.getEntity().getContentEncoding().getValue().equals("gzip"))
//			{
//				bytes = IOUtils.unZipGZ(bytes);
//			}
//			System.out.println(new String(bytes));
//		}
//	}

  test("Test-Cralwer") {

    case class HotelTask(hotelId: String) extends HttpCrawlerTask

    object TestCrawler extends DefaultHttpCrawler { 
      def processTask(httpTask: HttpCrawlerTask) {
        httpTask match {
          case HotelTask(hotelId) => logger.info("crawing " + hotelId)
          case _ => logger.error("not Hotel Task")
        }
      }

			def checkTask(task: HttpCrawlerTask): Boolean = true
    }

    TestCrawler.start()

    TestCrawler ! "Joke"

    case class StudentTask(studentId: String) extends HttpCrawlerTask
    TestCrawler ! StudentTask("1567")

    TestCrawler ! HotelTask("hotel-01801567")

    Thread.sleep(3 * 1000)

  }


}
