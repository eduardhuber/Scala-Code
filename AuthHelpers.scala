package controllers

import java.math.BigInteger
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import play.api.Configuration

trait AuthHelper {
  def configuration: Configuration

  lazy val ssoSecret = configuration.underlying.getString("discourse.ssoSecret")
  lazy val ssoUrl = configuration.underlying.getString("discourse.ssoUrl")

  def sha256(s: String):String = {
    val signingKey = new SecretKeySpec(ssoSecret.getBytes(), "HmacSHA256")
    val mac = Mac.getInstance("HmacSHA256")
    mac.init(signingKey)
    var digest = mac.doFinal(s.getBytes("UTF-8"))
    digest = mac.doFinal(s.getBytes())
    val bigInteger = new BigInteger(1,digest)
    String.format("%0" + (digest.length << 1) + "x", bigInteger)
  }

}
