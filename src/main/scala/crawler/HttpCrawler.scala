package jadeutils.crawler

import scala.actors.Actor


import org.apache.http.HttpResponse
import org.apache.http.NameValuePair
import org.apache.http.client.ClientProtocolException
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.HttpPost
import org.apache.http.client.methods.HttpUriRequest
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.impl.client.HttpClients
import org.apache.http.message.BasicNameValuePair

import jadeutils.common.Logging

abstract class HttpCrawlerTask

trait HttpCrawler extends Logging {

	def checkTask(task: HttpCrawlerTask): Boolean
	def processTask(task: HttpCrawlerTask)

	def sendRequest(request: HttpUriRequest, url:String, 
		headers:Map[String, String], params:Map[String, String]) 
	{
		// val pl = new java.util.ArrayList[NameValuePair]
		// params.foreach((e) => { pl.add(new BasicNameValuePair(e._1, e._2)) })
		// headers.foreach((e) => { request.setHeader(e._1, e._2) })

		// request.setEntity(new UrlEncodedFormEntity(params))

		// val httpclient = HttpClients.createDefault()
		// val resp: HttpResponse	= httpclient.execute(request)
		// val statusCode = resp.getStatusLine().getStatusCode()

		// if (statusCode >= 300 && statusCode < 400) {
		// 	System.out.println("重定向："
		// 		+ resp.getFirstHeader("Location").getValue());
		// } else if (statusCode == 200) {
		// 	import jadeutils.common.IOUtils

		// 	var bytes = IOUtils.readStream(resp.getEntity().getContent(), 2048)
		// 	if (resp.getEntity().getContentEncoding() != null
		// 		&& resp.getEntity().getContentEncoding().getValue().equals("gzip"))
		// 	{
		// 		bytes = IOUtils.unZipGZ(bytes);
		// 	}
		// 	System.out.println(new String(bytes));
		// }
	}

}

abstract class DefaultHttpCrawler extends Actor with HttpCrawler 
{

	def act() {
		while (true) {
			receive {
				case httpTask: HttpCrawlerTask => processTask(httpTask)
				case msg => logError("Error Msg Type: {}", msg)
			}
		}
	}

}
