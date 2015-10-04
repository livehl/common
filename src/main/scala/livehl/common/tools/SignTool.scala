package livehl.common.tools

import java.util.Arrays
import common.Tool

import scala.collection.convert.WrapAsJava._
import Tool._

/**
 * Created by A8 on 2015/7/11.
 */
object SignTool {
  def sortKeyAscMd5(map:Map[String,String],hasEmpty:Boolean,unSingKey:String*)={
    sortKeyAsc(map,hasEmpty,unSingKey:_ *).md5
  }

  def sortKeyAsc(map:Map[String,String],hasEmpty:Boolean,unSingKey:String*)={
    val key=map.keySet.toArray.filter(v=> !unSingKey.contains(v)).sorted
    val kvData=key.map(k=> k->map(k)).filter(kv=> if(hasEmpty) true else !isEmpty(kv._2)).map(kv=>kv._1+"="+kv._2).mkString("&")
    kvData
  }

}
