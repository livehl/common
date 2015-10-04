package livehl.common.tools

import java.io._

import com.aliyun.oss._
import com.aliyun.oss.model._

/**
 * Created by zixuan on 15-6-11.
 */
trait  FileSystem {
    def getFile(fileName:String): Option[Array[Byte]]
    def saveFile(fileName:String,data:Array[Byte]):Boolean
    def existFile(fileName:String):Boolean
    def deleteFile(fileName:String):Boolean
    def getFileUrl(fileName:String):String
    def getImageUrl(fileName:String,param:String):String
    def getParam(method:ImgMethod.Value,w:Int=0,h:Int=0):String

    def Byte2Stream(data:Array[Byte])={
      new ByteArrayInputStream(data)
    }
    def Stream2Byte(is: InputStream)={
      val baos=new  ByteArrayOutputStream
      var b = is.read()
      while (b != -1) {
        baos.write(b)
        b = is.read()
      }
      baos.toByteArray
    }
    def File2Byte(file: File):Array[Byte]={
      Stream2Byte(new FileInputStream(file))
    }
}

object ImgMethod extends Enumeration{
  val Left, Rigth, Cut,Per = Value
}
class Aliyun(val aid:String,val akey:String,val bucketName:String,val endpoint:String,val publicEndpoint:String) extends FileSystem{

  val client ={
    val conf = new ClientConfiguration()
    conf.setMaxConnections(100)
    conf.setConnectionTimeout(5000)
    conf.setMaxErrorRetry(3)
    conf.setSocketTimeout(2000)
    new OSSClient(endpoint,aid, akey,conf)
  }
  def getFile(fileName:String): Option[Array[Byte]]={
    try{
      Some(Stream2Byte(client.getObject(bucketName,akey).getObjectContent))
    }catch {
      case _:Throwable=> None
    }
  }
  def saveFile(fileName:String,data:Array[Byte],ct:String="")={
    val meta = new ObjectMetadata()
    // 必须设置ContentLength
    meta.setContentLength(data.length)
    if (null !=ct && !ct.isEmpty){
      meta.setContentType(ct)
      meta.setContentDisposition("")
    }
    // 上传Object.
    val result = client.putObject(bucketName, fileName, Byte2Stream(data), meta)
    true
  }
  def saveFile(fileName:String,data:Array[Byte])={
    saveFile(fileName,data,"")
  }
  def existFile(fileName:String):Boolean={
    try{
      client.getObjectMetadata(bucketName,akey).getETag
      true
    }catch {
      case _:Throwable=> false
    }
  }
    def deleteFile(fileName:String):Boolean={
      try{
        client.deleteObject(bucketName,akey)
        true
      }catch {
        case _:Throwable=> false
      }
    }
  def getFileUrl(fileName:String):String={
    "http://"+bucketName+"."+publicEndpoint+"/"+fileName
    "http://wxcdn.cdhub.cn/"+fileName
  }
  def getImageUrl(fileName:String,param:String):String={
    getFileUrl(fileName)+"@"+param
  }
  def getParam(method:ImgMethod.Value,w:Int,h:Int):String={
    method match {
      case ImgMethod.Left =>w+"w"
      case ImgMethod.Rigth =>h+"h"
      case ImgMethod.Cut =>h+"h_"+w+"w_1c"
      case ImgMethod.Per =>w+"p"
    }
  }

}
