package controllers

import java.time.LocalTime
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.parser.Parser
import play.api.Play.current
import play.api._
import play.api.libs.json
import play.api.data.Form
import play.api.data.Forms.{ mapping, nonEmptyText, boolean, number }
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

  val formItinerateQuery = Form(mapping(
    "type1" -> nonEmptyText,
    "name1" -> nonEmptyText,
    "type2" -> nonEmptyText,
    "name2" -> nonEmptyText,
    "reseau" -> nonEmptyText,
    "traveltype" -> nonEmptyText,
    "datestart" -> boolean,
    "datehour" -> number(min=0, max=23),
    "dateminute" -> number(min=0, max=59)
  )(ItineraryQuery.apply)(ItineraryQuery.unapply))

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

  case class Selector[A](f: Element => Option[A]) {
    def unapply(e: Element): Option[A] = f(e)
  }

  val rxSchedule = """\((\d{1,2}):(\d{1,2})\)""".r
  def selectorDate(rx: Regex) = Selector[LocalTime] { el =>
    import scala.collection.JavaConversions._
    Option(el)
      .filter { _.classNames.contains("bg2") }
      .filter { e => Option(e.children.first).map(_.text).flatMap(rx.findFirstIn).isDefined }
      .flatMap { e =>
        e.children().toSeq.lift(1).map(_.text).flatMap {
          case rxSchedule(hours, minutes) => Some(LocalTime.of(hours.toInt, minutes.toInt))
          case _ => None
        }
      }
  }

  val selectorStart = selectorDate("""Départ\s*:""".r)
  val selectorFinish = selectorDate("""Arrivée\s*:""".r)

  val rxSelectorWalk = """Aller à""".r
  val selectorWalk = Selector[String] { el =>
    import scala.collection.JavaConversions._
    Option(el)
      .filter { _.classNames.contains("bg1") }
      .filter { _.children.toSeq.lift(1).map(_.text).flatMap(rxSelectorWalk.findFirstIn).isDefined }
      .flatMap { _.textNodes.toSeq.lift(0).map(_.text) }
  }

  val rxSelectorDuration = """(\d+) min""".r
  def selectorDuration(klass: String) = Selector[LocalTime] { el =>
    import scala.collection.JavaConversions._
    Option(el)
      .filter { _.classNames.contains(klass) }
      .flatMap { e =>
        e.children().toSeq.lift(1).map(_.text).flatMap {
          case rxSelectorDuration(minutes) => Some(LocalTime.of(0, minutes.toInt))
          case _ => None
        }
      }
  }
  val selectorDuration1 = selectorDuration("bg1")
  val selectorDuration3 = selectorDuration("bg3")

  val rxSelectorDirection = """dir""".r
  val selectorDirection = Selector[String] { el =>
    import scala.collection.JavaConversions._
    Option(el)
      .filter { _.classNames.contains("bg3") }
      .filter { _.children.toSeq.lift(1).map(_.text).flatMap(rxSelectorDirection.findFirstIn).isDefined }
      .flatMap { _.textNodes.toSeq.lift(0).map(_.text) }
  }

  def selectorFromTo(rx: Regex) = Selector[(String, LocalTime)] { el =>
    import scala.collection.JavaConversions._
    Option(el)
      .filter { _.classNames.contains("bg3") }
      .filter { _.children.toSeq.lift(1).map(_.text).flatMap(rx.findFirstIn).isDefined }
      .flatMap { e =>
        for {
          station <- e.textNodes.toSeq.lift(0).map(_.text)
          date <- e.children().toSeq.lift(2).map(_.text).flatMap {
            case rxSchedule(hours, minutes) => Some(LocalTime.of(hours.toInt, minutes.toInt))
            case _ => None
          }
        } yield (station, date)
      }
  }

  val selectorFrom = selectorFromTo("""de""".r)
  val selectorTo = selectorFromTo("""à""".r)

  val rxSelectorCorrespondance = """Correspondance à""".r
  val selectorCorrespondance = Selector[String] { el =>
    import scala.collection.JavaConversions._
    Option(el)
      .filter { _.classNames.contains("bg1") }
      .filter { _.children.toSeq.lift(1).map(_.text).flatMap(rxSelectorCorrespondance.findFirstIn).isDefined }
      .flatMap { _.textNodes.toSeq.lift(0).map(_.text) }
  }

  case class ParsingCorrespondance(
    from: Option[String] = None,
    startAt: Option[LocalTime] = None,
    to: Option[String] = None,
    finishAt: Option[LocalTime] = None,
    line: Option[String] = None
  ) {
    def addFrom(station: String, date: LocalTime) = copy(from = Some(station), startAt = Some(date))
    def addTo(station: String, date: LocalTime) = copy(to = Some(station), finishAt = Some(date))
  }
  object ParsingCorrespondance {
    def empty = ParsingCorrespondance()
  }

  case class ParsingState(
    startedAt: Option[LocalTime] = None,
    finishedAt: Option[LocalTime] = None,
    inFirstWalk: Boolean = false,
    firstWalkDuration: Option[LocalTime] = None,
    correspondances: List[ParsingCorrespondance] = Nil  // in reverse order
  ) {
    def isFinished: Boolean = finishedAt.isDefined
    def started(date: LocalTime) = copy(startedAt = Some(date))
    def finished(date: LocalTime) = copy(finishedAt = Some(date))
    def addWalk(station: String) = {
      if (firstWalkDuration.isEmpty) copy(inFirstWalk = true)
      else this
    }
    def addDuration1(duration: LocalTime) = {
      if (inFirstWalk) copy(firstWalkDuration = Some(duration), inFirstWalk = false)
      else this
    }
    def addDirection(direction: String) = copy(correspondances = ParsingCorrespondance.empty :: correspondances)
    def addFrom(station: String, date: LocalTime) = {
      correspondances match {
        case c :: cs => copy(correspondances = c.addFrom(station, date) :: cs)
        case _ =>
          Logger.warn(s"Trying to add a From without having added Direction")
          this
      }
    }
    def addTo(station: String, date: LocalTime) = {
      correspondances match {
        case c :: cs => copy(correspondances = c.addTo(station, date) :: cs)
        case _ =>
          Logger.warn(s"Trying to add a To without having added Direction")
          this
      }
    }

    def format: Option[json.JsValue] = {
      for {
        _startedAt <- this.startedAt
        _finishedAt <- this.finishedAt
      } yield {
        val totalDuration = _finishedAt - _startedAt
        Json.obj(
          "itineraire" -> Json.obj(
            "nb_correspondances" -> correspondances.size,
            "duree_total" -> totalDuration,
            "duree_marche_avant_premiere_station" -> firstWalkDuration,
            "correspondances" -> json.JsArray(correspondances.reverse.map { c =>
              Json.obj(
                "heure_depart" -> c.startAt,
                "station_depart" -> c.from,
                "station_arrivée" -> c.to,
                "ligne" -> c.line
              )
            })
          )
        )
      }
    }
  }
  object ParsingState {
    def empty: ParsingState = ParsingState()
  }

  def parseDocument(doc: Document): ParsingState = {
    import scala.collection.JavaConversions._
    doc.select(".bg1,.bg2,.bg3").toSeq.foldLeft(ParsingState.empty) {
      case (s, _) if s.isFinished => s
      case (s, selectorStart(date)) => println(s"start = $date"); s.started(date)
      case (s, selectorFinish(date)) => println(s"finish = $date"); s.finished(date)
      case (s, selectorWalk(station)) => println(s"walk = $station"); s.addWalk(station)
      case (s, selectorDuration1(duration)) => println(s"duration1 = $duration"); s.addDuration1(duration)
      case (s, selectorDirection(direction)) => println(s"direction = $direction"); s.addDirection(direction)
      case (s, selectorFrom((station, date))) => println(s"de = $station at $date"); s.addFrom(station, date)
      case (s, selectorTo((station, date))) => println(s"à = $station at $date"); s.addTo(station, date)
      case (s, selectorDuration3(duration)) => println(s"duration3 = $duration"); s  // osef
      case (s, selectorCorrespondance(correspondance)) => println(s"correspondance = $correspondance"); s  // osef
      case (s, e) => println(s"unknwon = $e"); s
    }
  }

  def parse(body: HTMLBody, url: String): Option[json.JsValue] = {
    val doc: Document = Parser.parse(body, url)
    parseDocument(doc).format
  }

  def callAndParse(query: Query): Future[Result] = {
    call(query).map { body =>
      parse(body, query.url) match {
        case Some(js) => Ok(js)
        case None => NotFound(Json.obj("error" -> "Can't find infos"))
      }
    }
  }

  def index = Action.async { implicit req =>
    import play.api.i18n.Messages.Implicits._
    formItinerateQuery.bindFromRequest.fold(
      err => Future.successful {
        BadRequest(Json.obj("error" -> "bad request", "errors" -> err.errorsAsJson))
      },
      callAndParse
    )
  }

  def test = Action.async {
    callAndParse(ItineraryQuery(
      type1 = "station",
      name1 = "Danube",
      type2 = "station",
      name2 = "Pernety",
      reseau = "all",
      traveltype = "plus_rapide",
      datestart = true,
      datehour = 12,
      dateminute = 10
    ))
  }

}
