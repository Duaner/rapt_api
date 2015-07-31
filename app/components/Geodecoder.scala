package components

import play.api.Play.current
import play.api._
import play.api.libs.json
import play.api.libs.ws
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Geodecoder {

  val baseURL = "https://maps.googleapis.com/maps/api/geocode/json"

  val key = Play.configuration.getString("googlemap.key").filterNot(_.isEmpty).getOrElse {
    sys.error("Missing googlemap.key config")
  }

  def call(latlng: String): Future[json.JsValue] = {
    Logger.debug(s"CALL $baseURL?latlng=$latlng&key=$key")
    ws.WS
      .url(baseURL)
      .withQueryString("latlng" -> latlng, "key" -> key)
      .get()
      .flatMap {
        case resp if resp.status == 200 =>
          val res = resp.json
          (res \ "status").asOpt[String] match {
            case Some("OK") => Future.successful(res)
            case _ =>
              val msg = (res \ "error_message").asOpt[String].getOrElse(json.Json.stringify(res))
              Future.failed(UnexpectedResponse(200, msg))
          }
        case resp => Future.failed(UnexpectedResponse(resp))
      }
  }

  def searchAddress(latlng: String): Future[Option[String]] = {
    call(latlng).map { resp =>
      (resp \ "results" \\ "formatted_address").headOption.flatMap(_.asOpt[String])
    }
  }

}
