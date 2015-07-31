package controllers

import java.time.LocalTime
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.parser.Parser
import play.api.Play.current
import play.api._
import play.api.libs.json
import play.api.libs.json.Json
import play.api.libs.ws
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.matching.Regex

class Application extends Controller {

  trait Query {
    def toQueryString: Seq[(String, String)]
    def url: String
  }

  case class ItineraryQuery(
    type1: String,
    name1: String,
    type2: String,
    name2: String,
    reseau: String,
    traveltype: String,
    datestart: Boolean,
    datehour: Int,
    dateminute: Int
  ) extends Query {
    def url = ItineraryQuery.url
    def toQueryString = Seq[(String, String)](
      "type1" -> type1,
      "name1" -> name1,
      "type2" -> type2,
      "name2" -> name2,
      "reseau" -> reseau,
      "traveltype" -> traveltype,
      "datestart" -> datestart.toString,
      "datehour" -> datehour.toString,
      "dateminute" -> dateminute.toString
    )
  }
  object ItineraryQuery {
    val url = "http://wap.ratp.fr/siv/itinerary-list"
  }

  case class UnexpectedResponse(status: Int, content: String) extends Exception

  type HTMLBody = String

  def call(query: Query): Future[HTMLBody] = {
    ws.WS
     .url("http://wap.ratp.fr/siv/itinerary-list")
     .withQueryString(query.toQueryString: _*)
     .withHeaders("User-Agent" -> "super-user")
     .get()
     .flatMap {
       case resp if resp.status == 200 => Future.successful(resp.body: HTMLBody)
       case resp => Future.failed(UnexpectedResponse(resp.status, resp.body))
     }
  }

  implicit val jsonLocalTimeWrites = json.Writes[LocalTime] { lt =>
    Json.obj("hours" -> lt.getHour, "minutes" -> lt.getMinute)
  }
  implicit class PimpedLocalTime(val lt: LocalTime) {
    def -(other: LocalTime): LocalTime = {
      lt.minusHours(other.getHour).minusMinutes(other.getMinute)
    }
  }

  val rxSchedule = """\((\d{1,2}):(\d{1,2})\)""".r
  def findDate(doc: Document, where: Regex): Option[LocalTime] = {
    import scala.collection.JavaConversions._
    doc.select(".bg2").toSeq.find { el =>
      Option(el.children.first).map(_.text).flatMap(where.findFirstIn).isDefined
    }.flatMap { el =>
      el.children().toSeq.lift(1).map(_.text).flatMap {
        case rxSchedule(hours, minutes) => Some(LocalTime.of(hours.toInt, minutes.toInt))
        case _ => None
      }
    }
  }

  def parse(body: HTMLBody, url: String): Option[json.JsValue] = {
    val doc: Document = Parser.parse(body, url)
    for {
      start <- findDate(doc, """Départ\s*:""".r)
      finish <- findDate(doc, """Arrivée\s*:""".r)
    } yield {
      val totalDuration = finish - start
      Json.obj(
        "itineraire" -> Json.obj(
          "nb_correspondances" -> "",
          "duree_total" -> totalDuration,
          "duree_marche_avant_premiere_station" -> "",
          "correspondances" -> Json.arr(
            Json.obj(
              "heure_depart" -> start,
              "station_depart" -> "",
              "station_arrivée" -> "",
              "ligne" -> ""
            )
          )
        )
      )
    }
  }

  def index = Action.async {
    val query = ItineraryQuery(
      type1 = "station",
      name1 = "olympiades",
      type2 = "station",
      name2 = "saint-lazare",
      reseau = "all",
      traveltype = "plus_rapide",
      datestart = true,
      datehour = 10,
      dateminute = 20
    )
    call(query).map { body =>
      parse(body, query.url) match {
        case Some(js) => Ok(js)
        case None => NotFound(Json.obj("error" -> "Can't find infos"))
      }
    }
  }

}
