import cats.effect.*
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import org.http4s.*
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.`Content-Type`
import org.typelevel.log4cats.slf4j.Slf4jFactory

import scala.concurrent.duration.DurationInt

object HttpService {

  private given Slf4jFactory[IO] = Slf4jFactory.create[IO]

  extension (s: String) {
    def toUriUnsafe: Uri = Uri.fromString(s).getOrElse(???)
  }

  val client = EmberClientBuilder
    .default[IO]
    .withTimeout(5.minutes)
    .withIdleTimeInPool(6.minutes)
    .withIdleConnectionTime(6.minutes)
    .withMaxTotal(16)
    .build

  private def printError(e: Exception): Nothing = {
    println(e.getMessage)
    ???
  }

  private def s2j[M: Decoder](s: String): M = {
    summon[Decoder[M]]
      .decodeJson(parse(s).fold(printError, identity))
      .fold(printError, identity)
  }

  private def mkED[Rs: Decoder]: EntityDecoder[IO, Rs] = {
    summon[EntityDecoder[IO, String]].map(s2j)
  }

  def getJson[Rs: Decoder](uri: Uri): IO[Rs] = {
    given EntityDecoder[IO, Rs] = mkED

    val req = Request[IO](method = Method.GET, uri = uri)
    client.use { c =>
      c.expect[Rs](req)
    }
  }

  def postJson[Rq: Encoder, Rs: Decoder](uri: Uri,
                                         rq: Rq): IO[Rs] = {
    given EntityDecoder[IO, Rs] = mkED

    val req = Request[IO](
      method = Method.POST,
      uri = uri,
      headers = Headers(`Content-Type`(MediaType.application.json)),
      entity = Entity.utf8String(rq.asJson.noSpaces)
    )

    client.use { c =>
      c.expect[Rs](req)
    }
  }

  def postJsonGetBytes[Rq: Encoder](uri: Uri,
                                    rq: Rq): IO[Array[Byte]] = {
    val req = Request[IO](
      method = Method.POST,
      uri = uri,
      headers = Headers(`Content-Type`(MediaType.application.json)),
      entity = Entity.utf8String(rq.asJson.noSpaces)
    )

    client.use { c =>
      c.expect[Array[Byte]](req)
    }
  }


}
