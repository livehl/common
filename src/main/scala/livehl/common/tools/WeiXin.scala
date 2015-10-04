package livehl.common.tools

import java.security.MessageDigest
import java.util
import java.util.{Formatter, UUID}

import common.{Tool}
import org.apache.http.entity.ByteArrayEntity
import play.Logger

import scala.util.Random
import scala.util.parsing.json.JSON
import common.Tool._
import scala.collection.mutable

/**
 * Created by 林 on 14-4-11.
 */
class WeiXin(appid: String, secret: String,wxmchId:String="",payKey:String="") {

  val code_msg = Map("-1" -> "系统繁忙",
    "0" -> "请求成功",
    "40001" -> "获取access_token时AppSecret错误，或者access_token无效",
    "40002" -> "不合法的凭证类型",
    "40003" -> "不合法的OpenID",
    "40004" -> "不合法的媒体文件类型",
    "40005" -> "不合法的文件类型",
    "40006" -> "不合法的文件大小",
    "40007" -> "不合法的媒体文件id",
    "40008" -> "不合法的消息类型",
    "40009" -> "不合法的图片文件大小",
    "40010" -> "不合法的语音文件大小",
    "40011" -> "不合法的视频文件大小",
    "40012" -> "不合法的缩略图文件大小",
    "40013" -> "不合法的APPID",
    "40014" -> "不合法的access_token",
    "40015" -> "不合法的菜单类型",
    "40016" -> "不合法的按钮个数",
    "40017" -> "不合法的按钮个数",
    "40018" -> "不合法的按钮名字长度",
    "40019" -> "不合法的按钮KEY长度",
    "40020" -> "不合法的按钮URL长度",
    "40021" -> "不合法的菜单版本号",
    "40022" -> "不合法的子菜单级数",
    "40023" -> "不合法的子菜单按钮个数",
    "40024" -> "不合法的子菜单按钮类型",
    "40025" -> "不合法的子菜单按钮名字长度",
    "40026" -> "不合法的子菜单按钮KEY长度",
    "40027" -> "不合法的子菜单按钮URL长度",
    "40028" -> "不合法的自定义菜单使用用户",
    "40029" -> "不合法的oauth_code",
    "40030" -> "不合法的refresh_token",
    "40031" -> "不合法的openid列表",
    "40032" -> "不合法的openid列表长度",
    "40033" -> "不合法的请求字符",
    "40035" -> "不合法的参数",
    "40038" -> "不合法的请求格式",
    "40039" -> "不合法的URL长度",
    "40050" -> "不合法的分组id",
    "40051" -> "分组名字不合法",
    "41001" -> "缺少access_token参数",
    "41002" -> "缺少appid参数",
    "41003" -> "缺少refresh_token参数",
    "41004" -> "缺少secret参数",
    "41005" -> "缺少多媒体文件数据",
    "41006" -> "缺少media_id参数",
    "41007" -> "缺少子菜单数据",
    "41008" -> "缺少oauth code",
    "41009" -> "缺少openid",
    "42001" -> "access_token超时",
    "42002" -> "refresh_token超时",
    "42003" -> "oauth_code超时",
    "43001" -> "需要GET请求",
    "43002" -> "需要POST请求",
    "43003" -> "需要HTTPS请求",
    "43004" -> "需要接收者关注",
    "43005" -> "需要好友关系",
    "44001" -> "多媒体文件为空",
    "44002" -> "POST的数据包为空",
    "44003" -> "图文消息内容为空",
    "44004" -> "文本消息内容为空",
    "45001" -> "多媒体文件大小超过限制",
    "45002" -> "消息内容超过限制",
    "45003" -> "标题字段超过限制",
    "45004" -> "描述字段超过限制",
    "45005" -> "链接字段超过限制",
    "45006" -> "图片链接字段超过限制",
    "45007" -> "语音播放时间超过限制",
    "45008" -> "图文消息超过限制",
    "45009" -> "接口调用超过限制",
    "45010" -> "创建菜单个数超过限制",
    "45015" -> "回复时间超过限制",
    "45016" -> "系统分组，不允许修改",
    "45017" -> "分组名字过长",
    "45018" -> "分组数量超过上限",
    "46001" -> "不存在媒体数据",
    "46002" -> "不存在的菜单版本",
    "46003" -> "不存在的菜单数据",
    "46004" -> "不存在的用户",
    "47001" -> "解析JSON/XML内容错误",
    "48001" -> "api功能未授权",
    "50001" -> "用户未授权该api"
  )
  private var last_time: Long = 0
  private var token: String = ""
  private var expires = 0

  //主动发消息给用户
  def sendMsg(id: String, content: String): Boolean = {
    token = getToken
    val data = s"""{"touser":"${id}","msgtype":"text","text":{"content":"${content}"}}"""
    val entity = new ByteArrayEntity(data.getBytes("UTF-8"))
    val result_data = NetTool.HttpPost(s"https://api.weixin.qq.com/cgi-bin/message/custom/send?access_token=$token", entity = entity)._2
    val code = dealResult(result_data)
    if ((40001 :: 40014 :: 42001 :: Nil).exists(code == _)) {
      return sendMsg(id, content)
    }
    //    else if (code!=0 && (code !=45015))
    //      sendMail('微信邮件无法正常发出',result_data)
    return dealResult(result_data) == 0
  }

  //主动发图文消息给用户
  def sendNews(id: String, datas:List[Map[String,String]]): Boolean = {
    token = getToken
    val data = s"""{"touser":"${id}","msgtype":"news","news":{"articles":${datas.toJson}}}"""
    val entity = new ByteArrayEntity(data.getBytes("UTF-8"))
    val result_data = NetTool.HttpPost(s"https://api.weixin.qq.com/cgi-bin/message/custom/send?access_token=$token", entity = entity)._2
    val code = dealResult(result_data)
    if ((40001 :: 40014 :: 42001 :: Nil).exists(code == _)) {
      return sendNews(id, datas)
    }
    //    else if (code!=0 && (code !=45015))
    //      sendMail('微信邮件无法正常发出',result_data)
    return dealResult(result_data) == 0
  }

  //创建菜单
  def createMenu() = {
    val token = getToken()
    val data =
      """{"button":[{"name":"账户","sub_button":[{"name":"最新活动","type":"view","url":"https://www.cdhub.cn/html5/index.html"},{"name":"我的资产","type":"click","key":"wx_key_my_account"},{"name":"提现100","type":"click","key":"wx_key_qucik_withdrawal_100"},{"name":"提现","type":"view","url":"https://www.cdhub.cn/html5/withdrawal.html"},{"name":"充值","type":"view","url":"https://www.cdhub.cn/html5/charge.html"}]},
        |{"name":"定期理财","sub_button":[{"name":"投资列表","type":"click","key":"wx_key_loans"},{"name":"最近待收","type":"click","key":"wx_key_my_funds"},
        |{"name":"快速投资","type":"click","key":"wx_key_quick_tender"}]},{"name": "活期理财",
        |"sub_button": [{"name": "账户信息","type": "click","key": "wx_key_my_dbls"},
        |{"name": "转出100","type": "click","key": "wx_key_dbls_out_100"},
        |{"name": "转出500","type": "click","key": "wx_key_dbls_out_500"},
        |{"name": "全部转出","type": "click","key": "wx_key_dbls_out"},
        |{"name": "全部转入","type": "click","key": "wx_key_dbls_in"}]}]}""".stripMargin
    val entity = new ByteArrayEntity(data.getBytes("UTF-8"))
    val result_data = NetTool.HttpPost(s"https://api.weixin.qq.com/cgi-bin/menu/create?access_token=${token}", entity = entity)._2
    if (dealResult(result_data) != 0)
      throw new Exception(result_data)
    dealResult(result_data)
  }

  def moveGroup(uid: String, id: Int) {
    val token = getToken()
    val data = s"""{"openid":"${uid}","to_groupid":${id}"""
    val entity = new ByteArrayEntity(data.getBytes("UTF-8"))
    val result_data = NetTool.HttpPost(s"https://api.weixin.qq.com/cgi-bin/groups/members/update?access_token=${token}", entity = entity)._2
    if (dealResult(result_data) != 0)
      throw new Exception(result_data)
    dealResult(result_data)
  }
  def createGroup(name: String)={
    val token = getToken()
    val data = s"""{"group":{"name":"$name"}}"""
    val entity = new ByteArrayEntity(data.getBytes("UTF-8"))
    val result_data = NetTool.HttpPost(s"https://api.weixin.qq.com/cgi-bin/groups/create?access_token=${token}", entity = entity)._2
    val om=Map[String, Any]().toBean(result_data)
    println(om)
    om("group").asInstanceOf[Map[String,Any]]("id").toString
  }

  def userInfo(uid: String):Map[String,String] = {
    val token = getToken
    val result_data = NetTool.HttpGet(s"https://api.weixin.qq.com/cgi-bin/user/info?access_token=${token}&openid=${uid}&lang=zh_CN")._2
    if (result_data != null) {
      JSON.parseFull(result_data).getOrElse(Map("" -> "")).asInstanceOf[Map[String,String]]
    } else Map("" -> "")
  }

  def remark(uid: String, name: String) = {
    val token = getToken()
    val data = s"""{"openid":"${uid}","remark":"${name}"}"""
    val entity = new ByteArrayEntity(data.getBytes("UTF-8"))
    val result_data = NetTool.HttpPost(s"https://api.weixin.qq.com/cgi-bin/user/info/updateremark?access_token=${token}", entity = entity)._2
    if (dealResult(result_data) != 0)
      throw new Exception(result_data)
    dealResult(result_data)
  }

  //处理返回结果
  def dealResult(data: String): Int = {
    if (data != null) {
      JSON.parseFull(data) map {
        case m: Map[_,_] =>
          val code = m.asInstanceOf[Map[String,Any]]("errcode").toString.toDouble.toInt
          if ((42001 :: 41001 :: 40014 :: 40001 :: Nil).exists(code == _)) {
            last_time = 0
            getToken
          }
          return code
      }
    }
    return -1
  }

  //获取access_token
  def getToken() = {
    if (last_time == 0 || System.currentTimeMillis() / 1000 - last_time < expires) {
      val strdata = NetTool.HttpGet(s"https://api.weixin.qq.com/cgi-bin/token?grant_type=client_credential&appid=${appid}&secret=${secret}")
      JSON.parseFull(strdata._2) map {
        case m: Map[_, _] =>
          last_time = System.currentTimeMillis() / 1000
          expires = m.asInstanceOf[Map[String,Any]]("expires_in").toString.toDouble.toInt - 10
          token = m.asInstanceOf[Map[String,Any]]("access_token").toString
      }
    }
    token
  }

  //创建绑定二维码,100秒的失效时间
  def RealQrcode(code: String,limit:Boolean=false):String = {
    val data = if (!limit)
      s"""{"expire_seconds": 100, "action_name": "QR_SCENE", "action_info": {"scene": {"scene_id":${code}}}"""
    else
      s"""{"action_name": "QR_LIMIT_SCENE", "action_info": {"scene": {"scene_id":${code}}}"""
    val token = getToken()
    val entity = new ByteArrayEntity(data.getBytes("UTF-8"))
    val result_data = NetTool.HttpPost(s"https://api.weixin.qq.com/cgi-bin/qrcode/create?access_token=$token", entity = entity)._2
    Logger.info(result_data)
    JSON.parseFull(result_data) map {
      case m: Map[_, _] =>
        if (m.asInstanceOf[Map[String,Any]].getOrElse("errcode", null) != null) {
          last_time = 0
          getToken()
          return RealQrcode(code)
        } else {
          return "https://mp.weixin.qq.com/cgi-bin/showqrcode?ticket=" + m.asInstanceOf[Map[String,Any]]("ticket").toString
        }
    } orNull
  }

  def Qrcode(uid: String,limit:Boolean=false): (String, Int) = {
    val code = RandCode(uid)
    (RealQrcode(code,limit),code)
  }

  //创建绑定随机数,100秒的失效时间
  def RandCode(data: String, t: Int = 100): Int = {
    val code = Random.nextInt(1000000)
    val key = "wx_rand_code" + code
    if (Tool.getCache(key).isDefined)
      return RandCode(data, t)
    Tool.setCache(key, data,t)
    return code
  }
  //js签名
  def JsSignature(url: String) = {
    if (Tool.getCache(url).isEmpty) {
      val token = getToken()
      val strdata = NetTool.HttpGet(s"https://api.weixin.qq.com/cgi-bin/ticket/getticket?access_token=${token}&type=jsapi")
      JSON.parseFull(strdata._2) map {
        case m: Map[_,_] =>
          expires = m.asInstanceOf[Map[String,Any]]("expires_in").toString.toDouble.toInt - 10
          val ticket = m.asInstanceOf[Map[String,Any]]("ticket").toString
          Tool.setCache(url, ticket, expires)
      }
    }
    val jsticket = Tool.getCache(url).get.toString()
    sign(jsticket, url)
  }
  //js签名
  def sign(jsapi_ticket: String, url: String) = {
    val nonce_str: String = randomStr(6)
    val timestamp: String = System.currentTimeMillis() / 1000 toString
    var signature: String = ""
    val string1 = "jsapi_ticket=" + jsapi_ticket + "&noncestr=" + nonce_str + "&timestamp=" + timestamp + "&url=" + url
    val crypt: MessageDigest = MessageDigest.getInstance("SHA-1")
    crypt.reset
    crypt.update(string1.getBytes("UTF-8"))
    signature = bytes2hex(crypt.digest)
    Map("weixinAppid"->appid, "url" -> url, "jsapi_ticket" -> jsapi_ticket, "nonceStr" -> nonce_str, "timestamp" -> timestamp, "signature" -> signature)
  }

  def templateMessage(uid: String, template: String,url:String,map:Map[String,Map[String,String]]) = {
    val token = getToken()
    val data = s"""{"touser":"${uid}","template_id":"${template}","url":"${url}","topcolor":"#FF0000","data":${map.toJson}}"""
    println(data)
    val entity = new ByteArrayEntity(data.getBytes("UTF-8"))
    val result_data = NetTool.HttpPost(s"https://api.weixin.qq.com/cgi-bin/message/template/send?access_token=${token}", entity = entity)._2
    if (dealResult(result_data) != 0)
      throw new Exception(result_data)
    dealResult(result_data)
  }

  def rechargeWxPay(title:String,ordeId:String,amount:Int,ip:String,notifyUrl:String,openid:String)={
    val params=new mutable.HashMap[String,String]()
    params("appid")=appid
    params("mch_id")=wxmchId
    params("nonce_str")=Tool.randomStr(20)
    params("body")=title
    params("out_trade_no")=ordeId
    params("total_fee")=amount.toString
    params("spbill_create_ip")=ip
    params("notify_url")=notifyUrl
    params("trade_type")="JSAPI"
    params("openid")=openid
//    params("device_info")="WEB"
    params("sign")=(SignTool.sortKeyAsc(params.toMap,false)+"&key="+payKey).md5
    params
  }

  def checkSignWxPay(params:Map[String,String])={
    val sign=params("sign")
    (SignTool.sortKeyAsc(params.filter(_._1!="sign"),false)+"&key="+payKey).md5
  }

  def genWxHtmlPay(id:String)={
    val params=new mutable.HashMap[String,String]()
    params("appId")=appid
    params("timeStamp")=(System.currentTimeMillis()/1000).toString
    params("nonceStr")=Tool.randomStr(20)
    params("package")="prepay_id="+id
    params("signType")="MD5"
    params("paySign")=(SignTool.sortKeyAsc(params.toMap,false)+"&key="+payKey).md5
    params
  }



}