package controllers

import org.jsoup.parser.Parser
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import play.api.Play.current
import play.api._
import play.api.libs.ws
import play.api.libs.json
import play.api.libs.json.Json
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

  trait Response {
    def toJson: json.JsValue
  }

  type Hours = Int
  type Minutes = Int
  case class Schedule(hours: Hours, minutes: Minutes)
  object Schedule {
    implicit val jsonWrites = json.Writes[Schedule] { schedule =>
      Json.obj("hours" -> schedule.hours, "minutes" -> schedule.minutes)
    }
  }

  val rxSchedule = """\((\d{1,2}):(\d{1,2})\)""".r
  def findDate(doc: Document, where: Regex): Option[Schedule] = {
    import scala.collection.JavaConversions._
    doc.select(".bg2").toSeq.find { el =>
      Option(el.children.first).map(_.text).flatMap(where.findFirstIn).isDefined
    }.flatMap { el =>
      el.children().toSeq.lift(1).map(_.text).flatMap {
        case rxSchedule(hours, minutes) => Some(Schedule(hours.toInt, minutes.toInt))
        case _ => None
      }
    }
  }

  def parse(body: HTMLBody, url: String): Response = {

    val doc: Document = Parser.parse(body, url)
    val start = findDate(doc, """Départ\s*:""".r)
    val finish = findDate(doc, """Arrivée\s*:""".r)

    new Response {
      def toJson = Json.obj(
        "start" -> start,
        "finish" -> finish
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
      val resp = parse(body, query.url)
      Ok(resp.toJson)
    }
  }

}
