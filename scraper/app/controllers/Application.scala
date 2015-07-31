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

  case class ParsingState(isFinished: Boolean) {
    def finished = copy(isFinished = true)
  }
  object ParsingState {
    def empty = ParsingState(isFinished = false)
    object unfinished {
      def unapply(s: ParsingState) = if (s.isFinished) None else Some(s)
    }
  }

  def parseDocument(doc: Document) = {
    import scala.collection.JavaConversions._
    doc.select(".bg1,.bg2,.bg3").toSeq.fold(ParsingState.empty) {
      case (ParsingState.unfinished(s), selectorStart(date)) => println(s"start = $date"); s
      case (ParsingState.unfinished(s), selectorFinish(date)) => println(s"finish = $date"); s.finished
      case (ParsingState.unfinished(s), selectorWalk(station)) => println(s"walk = $station"); s
      case (ParsingState.unfinished(s), selectorDuration1(duration)) => println(s"duration1 = $duration"); s
      case (ParsingState.unfinished(s), selectorDirection(direction)) => println(s"direction = $direction"); s
      case (ParsingState.unfinished(s), selectorFrom((station, date))) => println(s"def = $station at $date"); s
      case (ParsingState.unfinished(s), selectorTo((station, date))) => println(s"à = $station at $date"); s
      case (ParsingState.unfinished(s), selectorDuration3(duration)) => println(s"duration3 = $duration"); s
      case (ParsingState.unfinished(s), selectorCorrespondance(correspondance)) => println(s"correspondance = $correspondance"); s
      case (ParsingState.unfinished(s), e) => println(s"unknwon = $e"); s
      case (s, _) => s
    }
  }

  def parse(body: HTMLBody, url: String): Option[json.JsValue] = {
    val doc: Document = Parser.parse(body, url)
    parseDocument(doc)
    Some(Json.obj())
    // for {
    //   start <- findDate(doc, """Départ\s*:""".r)
    //   finish <- findDate(doc, """Arrivée\s*:""".r)
    // } yield {
    //   val totalDuration = finish - start
    //   Json.obj(
    //     "itineraire" -> Json.obj(
    //       "nb_correspondances" -> "",
    //       "duree_total" -> totalDuration,
    //       "duree_marche_avant_premiere_station" -> "",
    //       "correspondances" -> Json.arr(
    //         Json.obj(
    //           "heure_depart" -> start,
    //           "station_depart" -> "",
    //           "station_arrivée" -> "",
    //           "ligne" -> ""
    //         )
    //       )
    //     )
    //   )
    // }
  }

  def index = Action.async {
    val query = ItineraryQuery(
      type1 = "station",
      name1 = "Danube",
      type2 = "station",
      name2 = "Pernety",
      reseau = "all",
      traveltype = "plus_rapide",
      datestart = true,
      datehour = 12,
      dateminute = 10
    )
    call(query).map { body =>
      parse(body, query.url) match {
        case Some(js) => Ok(js)
        case None => NotFound(Json.obj("error" -> "Can't find infos"))
      }
    }
  }

}
