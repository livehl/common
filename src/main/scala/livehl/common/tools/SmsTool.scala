package livehl.common.tools

import java.net.URLEncoder
import scala.io.Source
import scala.util.parsing.json.JSON
import org.apache.http.entity.ByteArrayEntity
import scala.xml.XML
import scala.concurrent._
import ExecutionContext.Implicits.global
import org.apache.commons.codec.binary.{Base64=>B64}
import java.text.SimpleDateFormat
import java.util.Date
import common.Tool._

/**
 * Created by 林 on 14-4-10.
 */
trait BaseSms {
  def sendSms(to: String, content: String): Boolean

  def sendSms(msgs: List[(String, String)]): Boolean

  def sendVerify(to: String, content: String, code: Int): Boolean

  def queryBalance: Int
}

//漫道短信通道
class ManDao(sdk: String, pwd: String) extends BaseSms {
  val password = md5(sdk + pwd)
  val baseUrl = "http://sdk2.entinfo.cn:8061/webservice.asmx"

  //发送通道
  def sendSms(to: String, content: String): Boolean = {
    val data = Map("sn" -> sdk, "pwd" -> password, "mobile" -> to, "content" -> content,
      "ext" -> "", "stime" -> "", "rrid" -> "", "msgfmt" -> "")
    val result = NetTool.HttpPost(baseUrl + "/mdsmssend", appendHead = Map("Host" -> "sdk.entinfo.cn"), data = data)._2
    (XML.loadString(result).child(0).toString) != "0"
  }

  def sendVerify(to: String, content: String, code: Int): Boolean = {
    sendSms(to, code)
  }

  def sendSms(msgs: List[(String, String)]): Boolean = {
    val results=msgs.grouped(800) map {
      list =>
        val data = Map("sn" -> sdk, "pwd" -> password, "mobile" -> (list map (_._1) mkString (",")),
          "content" -> (list map (d => URLEncoder.encode(d._2, "utf-8")) mkString (",")),
          "ext" -> "", "stime" -> "", "rrid" -> "", "msgfmt" -> "")
        val result=NetTool.HttpPost(baseUrl + "/mdgxsend", appendHead = Map("Host" -> "sdk.entinfo.cn"), data = data)._2
        (XML.loadString(result).child(0).toString) != "0"
    }
    results.filter(!_).isEmpty
  }

  def queryBalance: Int = {
    val result = NetTool.HttpPost("http://sdk.entinfo.cn:8060/webservice.asmx/balance", data = Map("sn" -> sdk, "pwd" -> password), appendHead = Map("Content-Type" -> "application/x-www-form-urlencoded", "Host" -> "sdk.entinfo.cn"))._2
    (XML.loadString(result).child(0).toString) toInt
  }
}


//云通讯短信通道
class Cloopen(aid: String, atoken: String, appid: String) extends BaseSms {
  val baseUrl = "https://app.cloopen.com:8883/2013-12-26"
  val appendHead = Map("Accept" -> "application/json", "Content-Type" -> "application/json;charset=utf-8")
  lazy val sid = getSid()

  //发送通道
  private def request(url: String, id: String, token: String, data: Map[String, String] = null) = {
    val time = new SimpleDateFormat("yyyyMMddHHmmss").format(new Date())
    val auth = B64.encodeBase64String(s"$id:$time" getBytes)
    val sig = md5(id + token + time)
    if (data != null) {
      val entity = new ByteArrayEntity(data toJson() getBytes)
      NetTool.HttpPost(baseUrl + url + "?sig=" + sig, appendHead = appendHead + ("Authorization" -> auth), entity = entity)._2
    } else {
      NetTool.HttpGet(baseUrl + url + "?sig=" + sig, appendHead = appendHead + ("Authorization" -> auth))._2
    }

  }

  def sendSms(to: String, content: String): Boolean = {
    val data = Map("subAccountSid" -> sid, "appId" -> appid, "to" -> to, "body" -> content,
      "msgType" -> "0")
    val result = request(s"/Accounts/${aid}/SMS/Messages", aid, atoken, data = data)
    println(result)
    JSON.parseFull(result) map {
      case m: Map[_, _] =>
        try {
          m.asInstanceOf[Map[String,Any]]("SMSMessage").asInstanceOf[Map[String, Any]]("smsMessageSid")
          true
        } catch {
          case _:Throwable => false
        }
      case _ => false
    } getOrElse (false)
  }

  def sendVerify(to: String, content: String, code: Int): Boolean = {
    val data = Map("appId" -> appid, "verifyCode" -> code.toString, "playTimes" -> "3", "to" -> to)
    val result = request(s"/Accounts/${aid}/Calls/VoiceVerify", aid, atoken, data = data)
    println(result)
    JSON.parseFull(result) map {
      case m: Map[_, _] =>
        try {
          m.asInstanceOf[Map[String,Any]]("VoiceVerify").asInstanceOf[Map[String, Any]]("callSid")
          true
        } catch {
          case _:Throwable => false
        }
      case _ => false
    } getOrElse (false)
  }

  def sendSms(msgs: List[(String, String)]): Boolean = {
    msgs grouped (90) foreach (list => Future(sendSms(list.map(_._1).mkString(","), list.map(_._2).mkString(","))))
    true
  }

  def queryBalance: Int = {
    val result = request(s"/Accounts/$aid/AccountInfo", aid, atoken)
    println(result)
    JSON.parseFull(result) map {
      case m: Map[_, _] =>
        (m.asInstanceOf[Map[String,Any]]("Account").asInstanceOf[Map[String, Any]]("balance").toString.toDouble) * 10 toInt
      case _ => 0
    } getOrElse (0)
  }

  def getSid(): String = {
    val result = request(s"/Accounts/${aid}/GetSubAccounts", aid, atoken, Map("appId" -> appid))
    println(result)
    JSON.parseFull(result) map {
      case m: Map[_, _] =>
        m.asInstanceOf[Map[String,Any]]("SubAccount").asInstanceOf[List[Map[String, Any]]](0)("subAccountSid").toString
      case _ => null
    } getOrElse (null)
  }
}

//亿美短信通道
class YiMei(sdk: String, pwd: String) extends BaseSms {
  val baseUrl = "http://sdk4report.eucp.b2m.cn:8080/sdkproxy/"

  //发送通道
  def sendSms(to: String, content: String): Boolean = {
    // 处理消息前置的问题
    val recvExpr = getExpr(content, "【", "】", "").get
    val txt=recvExpr+content.replace(recvExpr,"")
    val data = Map("cdkey" -> sdk, "password" -> pwd, "phone" -> to, "message" -> txt)
    val result = NetTool.HttpPost(baseUrl + "sendsms.action", data = data)._2
    (XML.loadString(result.trim) \\ "error" text).toInt == 0
  }

  def sendVerify(to: String, content: String, code: Int): Boolean = {
    sendSms(to, content)

    //    val xml=s"""<?xml version="1.0"?>
    //              |<soap:Envelope
    //              |xmlns:soap="http://www.w3.org/2001/12/soap-envelope"
    //              |soap:encodingStyle="http://www.w3.org/2001/12/soap-encoding">
    //              |
    //              |  <soap:Body xmlns:m="http://www.example.org/stock">
    //              |    <tns:sendVoice>
    //              |      <arg0>$sdk</arg0>
    //              |      <arg1>$pwd</arg1>
    //              |      <arg2></arg2>
    //              |      <arg3>$to</arg3>
    //              |      <arg4>$code</arg4>
    //              |      <arg5></arg5>
    //              |      <arg6>utf-8</arg6>
    //              |      <arg7>5</arg7>
    //              |      <arg8></arg8>
    //              |    </tns:sendVoice>
    //              |  </soap:Body>
    //              |</soap:Envelope>"""
    //    val entity=new ByteArrayEntity(xml.getBytes())
    //    val result=NetTool.HttpPost("http://sdk4report.eucp.b2m.cn:8080/sdk/SDKService",appendHead = Map("Host"->"sdkhttp.eucp.b2m.cn"),entity = entity)._2
    //    println(result)
    //    (XML.loadString(result).child(0).toString).toLong > 1
  }

  def sendSms(msgs: List[(String, String)]): Boolean = {
    msgs.par.foreach(kv => safe {
      sendSms(kv._1, kv._2)
    })
    true
  }

  def queryBalance: Int = {
    val result = NetTool.HttpPost(baseUrl + "querybalance.action", data = Map("cdkey" -> sdk, "password" -> pwd))._2
    (XML.loadString(result.trim) \\ "message" text).toDouble.toInt
  }

  def reg: Boolean = {
    val data = Map("cdkey" -> sdk, "password" -> pwd)
    val result = NetTool.HttpPost(baseUrl + "regist.action", data = data)._2
    (XML.loadString(result.trim) \\ "error" text).toInt == 0
  }
}
