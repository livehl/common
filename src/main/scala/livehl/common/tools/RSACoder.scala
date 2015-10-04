package livehl.common.tools

import java.security.Key
import java.security.KeyFactory
import java.security.PrivateKey
import java.security.PublicKey
import java.security.Signature
import java.security.spec.PKCS8EncodedKeySpec
import java.security.spec.X509EncodedKeySpec
import javax.crypto.Cipher

//import org.bouncycastle.util.encoders.Base64;
class RSACoder(val KEY_ALGORITHM: String = "RSA",val  transformation: String = "RSA",val SIGNATURE_ALGORITHM: String = "SHA1withRSA") {

  /**
   * 私钥解密
   *
   * @param encryptedData
     * 已加密数据
   * @param privateKey
     * 私钥(BASE64编码)
   * @return the byte[]
   * @throws Exception
     * the exception
   * @author 黄林
   */
  def decryptByPrivateKey(encryptedData: Array[Byte], privateKey: String): Array[Byte] = {
    val keyBytes: Array[Byte] = Base64.decode(privateKey)
    val pkcs8KeySpec: PKCS8EncodedKeySpec = new PKCS8EncodedKeySpec(keyBytes)
    val keyFactory: KeyFactory = KeyFactory.getInstance(KEY_ALGORITHM)
    val privateK: Key = keyFactory.generatePrivate(pkcs8KeySpec)
    val cipher: Cipher = Cipher.getInstance(transformation)
    cipher.init(Cipher.DECRYPT_MODE, privateK)
    return cipher.doFinal(encryptedData)
  }

  /**
   * 公钥解密
   *
   * @param encryptedData
     * 已加密数据
   * @param publicKey
     * 公钥(BASE64编码)
   * @return the byte[]
   * @throws Exception
     * the exception
   * @author 黄林
   */
  def decryptByPublicKey(encryptedData: Array[Byte], publicKey: String): Array[Byte] = {
    val keyBytes: Array[Byte] = Base64.decode(publicKey)
    val x509KeySpec: X509EncodedKeySpec = new X509EncodedKeySpec(keyBytes)
    val keyFactory: KeyFactory = KeyFactory.getInstance(KEY_ALGORITHM)
    val publicK: Key = keyFactory.generatePublic(x509KeySpec)
    val cipher: Cipher = Cipher.getInstance(keyFactory.getAlgorithm)
    cipher.init(Cipher.DECRYPT_MODE, publicK)
    return cipher.doFinal(encryptedData)
  }

  /**
   * 私钥加密
   *
   * @param data
     * 源数据
   * @param privateKey
     * 私钥(BASE64编码)
   * @return the byte[]
   * @throws Exception
     * the exception
   * @author 黄林
   */
  def encryptByPrivateKey(data: Array[Byte], privateKey: String): Array[Byte] = {
    val keyBytes: Array[Byte] = Base64.decode(privateKey)
    val pkcs8KeySpec: PKCS8EncodedKeySpec = new PKCS8EncodedKeySpec(keyBytes)
    val keyFactory: KeyFactory = KeyFactory.getInstance(KEY_ALGORITHM)
    val privateK: Key = keyFactory.generatePrivate(pkcs8KeySpec)
    val cipher: Cipher = Cipher.getInstance(keyFactory.getAlgorithm)
    cipher.init(Cipher.ENCRYPT_MODE, privateK)
    return cipher.doFinal(data)
  }

  /**
   * 公钥加密
   *
   * @param data
     * 源数据
   * @param publicKey
     * 公钥(BASE64编码)
   * @return the byte[]
   * @throws Exception
     * the exception
   * @author 黄林
   */
  def encryptByPublicKey(data: Array[Byte], publicKey: String): Array[Byte] = {
    val keyBytes: Array[Byte] = Base64.decode(publicKey)
    val x509KeySpec: X509EncodedKeySpec = new X509EncodedKeySpec(keyBytes)
    val keyFactory: KeyFactory = KeyFactory.getInstance(KEY_ALGORITHM)
    val publicK: Key = keyFactory.generatePublic(x509KeySpec)
    val cipher: Cipher = Cipher.getInstance(keyFactory.getAlgorithm)
    cipher.init(Cipher.ENCRYPT_MODE, publicK)
    return cipher.doFinal(data)
  }

  def main(args: String*) {
    try {

    }
    catch {
      case e: Exception => {
        e.printStackTrace
      }
    }
  }

  /**
   * 用私钥对信息生成数字签名
   *
   * @param data
     * 已加密数据
   * @param privateKey
     * 私钥(BASE64编码)
   * @return the string
   * @throws Exception
     * the exception
   * @author 黄林
   */
  def sign(data: Array[Byte], privateKey: String): String = {
    val keyBytes: Array[Byte] = Base64.decode(privateKey)
    val pkcs8KeySpec: PKCS8EncodedKeySpec = new PKCS8EncodedKeySpec(keyBytes)
    val keyFactory: KeyFactory = KeyFactory.getInstance(KEY_ALGORITHM)
    val privateK: PrivateKey = keyFactory.generatePrivate(pkcs8KeySpec)
    val signature: Signature = Signature.getInstance(SIGNATURE_ALGORITHM)
    signature.initSign(privateK)
    signature.update(data)
    return new String(Base64.encode(signature.sign))
  }

  /**
   * 校验数字签名
   *
   * @param data
     * 已加密数据
   * @param publicKey
     * 公钥(BASE64编码)
   * @param sign
     * 数字签名
   * @return true, if successful
   * @throws Exception
     * the exception
   * @author 黄林
   */
  def verify(data: Array[Byte], publicKey: String, sign: String): Boolean = {
    val keyBytes: Array[Byte] = Base64.decode(publicKey)
    val keySpec: X509EncodedKeySpec = new X509EncodedKeySpec(keyBytes)
    val keyFactory: KeyFactory = KeyFactory.getInstance(KEY_ALGORITHM)
    val publicK: PublicKey = keyFactory.generatePublic(keySpec)
    val signature: Signature = Signature.getInstance(SIGNATURE_ALGORITHM)
    signature.initVerify(publicK)
    signature.update(data)
    return signature.verify(Base64.decode(sign))
  }

}