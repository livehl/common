package livehl.common.db

import java.text.SimpleDateFormat
import java.util.Date

import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

/**
 * Created by 林 on 14-3-26.
 */

class BaseDBEntity[+Self <: BaseDBEntity[Self]](tableName: String) extends DBEntity(tableName) {

  def toJson: String = {
    BaseDBEntity.map.writeValueAsString(this)
  }

  def toMap: Map[String, Any] = {
    BaseDBEntity.map.readValue(toJson, Map[String, Any]().getClass).asInstanceOf[Map[String, Any]]
  }

  def fromJson(json: String): Self = {
    BaseDBEntity.map.readValue(json, this.getClass).asInstanceOf[Self]
  }

  //将对应的更新类转为实体类
  def changeUpdateBean(): Self = {
    fromJson(toJson)
  }

  override def queryById(id: Long): Option[Self] = {
    super.queryById(id) map (_.asInstanceOf[Self])
  }

  override def queryByIds(ids: List[Long]): List[Self] = {
    super.queryByIds(ids) map (_.asInstanceOf[Self])
  }


  override def queryOne(sql: String, param: String*): Option[Self] = {
    super.queryOne(s"select * from $tableName where " + sql, param: _*) map (_.asInstanceOf[Self])
  }

  override def queryAll(): List[Self] = {
    super.queryAll map (_.asInstanceOf[Self])
  }

  override def query(where: String, param: AnyRef*): List[Self] = {
    super.query(where, param: _*) map (_.asInstanceOf[Self])
  }

  //这个接口需要传条件、排序
  override def queryPage(where: String, pageNum: Int, pageSize: Int, order: String, param: AnyRef*): (Int, List[Self]) = {
    val (count, list) = super.queryPage(where, pageNum, pageSize, order, param: _*)
    (count, list map (_.asInstanceOf[Self]))
  }

}

object BaseDBEntity {
  protected val map = new ObjectMapper() with ScalaObjectMapper
  map.registerModule(DefaultScalaModule)
  map.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false)
  map.setDateFormat(new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"))


  def toJson(data: AnyRef) = {
    map.writeValueAsString(data)
  }

  //自动检查数据的查询结果是否存在
  implicit class DBOptionAdd[T <: DBEntity](o: Option[T]) {
    def dbCheck: T = if (o.isEmpty) throw new DataNoFindExcepiton else o.get
  }

}


/**
 * 公共账号会员提交的文章链接信息
 *
 * @author zql
 */
class ArticleInfo(
                   val id: String = "",

                   val title: String = "",
                   val update_time: Date = new Date(),

                   /**
                    * 微信公众号会员发送的url路径
                    */
                   val member_url: String = "",
                 val mobile_content_path:String="",

                   /**
                    * 文章绑定的名片id
                    */
                   val card_info: String = "",
                   val pv: Int = 0,


                   val bottom_banner_type: BigDecimal = 0,
                   val bottom_banner_id: String = "",
                   val top_banner_type: BigDecimal = 0,
                   val top_banner_id: String = "",

                   /**
                    * 分享用的缩略图
                    */
                   val articleAvator: String = ""

                   ) extends BaseDBEntity[ArticleInfo]("ARTICLE_INFO")

class ShareArticleAd(

                      val self_ad_type: BigDecimal = 0, // 自营销广告类型：1="",2="",3="",4


                      val card_app_id: String = "",

                      val card_id: String = "",

                      val name: String = "", //广告名称

                      val description: String = "", //广告语(坏命名)

                      val ad_picture: String = "", //用户上传的背景图片

                      val qrcode_picture: String = "", //用户上传的二维码

                      val template: Int = 0, //0:表示用户上传图片="", 1~3:表示内置广告图片

                      val link: String = "", //广告链接
                      val id: String = "",
                      val create_time: Date = new Date() // 创建时间

                      ) extends BaseDBEntity[ShareArticleAd]("share_article_ad")


class CardInfo(
                val id: String = "",
                /**
                 * 姓名
                 */
                val name: String = "",
                /**
                 * 头像路径
                 */
                val avator: String = "",
                val specific_desc: String = "",
                val article_share_avatar: String = "",
                val use_article_share_avatar: String = "",
                val article_icon_list: String = "",
                val create_time:Date=new Date()
                ) extends BaseDBEntity[CardInfo]("CARD_INFO")

class ShareArticleConfig(
                          val id: String = "",
                          val article_share_avatar: String = "",
                          val use_article_share_avatar: String = "",
                          val article_icon_list: String = "") extends BaseDBEntity[ShareArticleConfig]("CARD_INFO") {

  override def toJson() = {
    val mapData = Map("articleShareAvatar" -> article_share_avatar, "name" -> use_article_share_avatar, "articleIconList" -> article_icon_list)
    BaseDBEntity.toJson(mapData)
  }


}


class Setting(val id: String = "", val value: String = "") extends BaseDBEntity[Setting]("Setting")
//统计信息
class PageView(val id:Int= -1,val click:Long=0,val uv:Long=0,val pv:Long=0,val ip:Long=0)extends BaseDBEntity[PageView]("PageView")
//展示日志
class PageViewLog(val id:Int= -1,val pageId:String="",val ip:String="",val ua:String="",val hostName:String="", val createDate:Date=new Date())extends BaseDBEntity[PageViewLog]("PageViewLog")


class AritcleContent(val id:String="",val title:String="",val content:String="") extends  BaseDBEntity[AritcleContent]("AritcleContent")


