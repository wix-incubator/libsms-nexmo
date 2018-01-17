package com.wix.sms.nexmo.testkit

import java.util.concurrent.atomic.AtomicReference
import java.util.{List => JList}

import akka.http.scaladsl.model._
import com.google.api.client.http.UrlEncodedParser
import com.wix.e2e.http.RequestHandler
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory.aMockWebServerWith
import com.wix.sms.model.Sender
import com.wix.sms.nexmo.model.{Message, Response, ResponseParser, Statuses}
import com.wix.sms.nexmo.{Credentials, NexmoHelper}

import scala.collection.JavaConversions._
import scala.collection.mutable

class NexmoDriver(port: Int) {
  private val delegatingHandler: RequestHandler = { case r: HttpRequest => handler.get().apply(r) }
  private val notFoundHandler: RequestHandler = { case _: HttpRequest => HttpResponse(status = StatusCodes.NotFound) }

  private val handler = new AtomicReference(notFoundHandler)

  private val probe = aMockWebServerWith(delegatingHandler).onPort(port).build
  private val responseParser = new ResponseParser

  def startProbe() {
    probe.start()
  }

  def stopProbe() {
    probe.stop()
  }

  def resetProbe() {
    handler.set(notFoundHandler)
  }

  def anSmsFor(credentials: Credentials, sender: Sender, destPhone: String, text: String): SmsCtx = {
    new SmsCtx(
      credentials = credentials,
      sender = sender,
      destPhone = destPhone,
      text = text)
  }

  private def prependHandler(handle: RequestHandler) =
    handler.set(handle orElse handler.get())

  class SmsCtx(credentials: Credentials, sender: Sender, destPhone: String, text: String) {
    private val expectedParams = NexmoHelper.createRequestParams(
      sender = sender,
      destPhone = destPhone,
      text = text,
      credentials = credentials
    )

    def returns(msgId: String): Unit = {
      val response = Response(
        `message-count` = "1",
        messages = Seq(Message(
          status = Statuses.success,
          to = Some(destPhone.substring(1)),
          `message-id` = Some(msgId),
          `remaining-balance` = Some("9.07060000"),
          `message-price` = Some("0.01040000"),
          network = Some("42501")
        ))
      )

      val responseJson = responseParser.stringify(response)
      returnJson(responseJson)
    }

    def failsWith(status: String, errorText: String): Unit = {
      val response = new Response(
        `message-count` = "1",
        messages = Seq(Message(
          status = status,
          `error-text` = Some(errorText)
        ))
      )

      val responseJson = responseParser.stringify(response)
      returnJson(responseJson)
    }

    def isUnauthorized(): Unit = {
      failsWith(
        status = Statuses.invalidCredentials,
        errorText = "Bad Credentials"
      )
    }

    private def returnJson(responseJson: String): Unit = {
      prependHandler({
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path("/sms/json"),
        _,
        entity,
        _) if isStubbedRequestEntity(entity) => HttpResponse(
          status = StatusCodes.BadRequest,
          entity = HttpEntity(ContentTypes.`application/json`, responseJson)
        )
      })
    }

    private def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      val requestParams = urlDecode(entity.extractAsString)

      requestParams == expectedParams
    }

    private def urlDecode(str: String): Map[String, String] = {
      val params = mutable.LinkedHashMap[String, JList[String]]()
      UrlEncodedParser.parse(str, mutableMapAsJavaMap(params))
      params.mapValues( _.head ).toMap
    }
  }
}
