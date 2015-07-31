package components

import java.time.LocalTime
import org.jsoup.nodes.Document
import org.jsoup.nodes.Element
import org.jsoup.parser.Parser
import play.api._
import play.api.libs.json
import play.api.libs.json.Json
import play.api.libs.ws
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.matching.Regex

object Scraper {

  trait Query {
    def toQueryString: Seq[(String, String)]
    def url: String
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
        e.children.toSeq.lift(1).map(_.text).flatMap {
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
        e.children.toSeq.lift(1).map(_.text).flatMap {
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

  val rxSelectorFromFilter = """de""".r
  val rxSelectorFromParserLine = """\[(.*)\]""".r
  val selectorFrom = Selector[(String, LocalTime, String)] { el =>
    import scala.collection.JavaConversions._
    Option(el)
      .filter { _.classNames.contains("bg3") }
      .filter { _.children.toSeq.lift(1).map(_.text).flatMap(rxSelectorFromFilter.findFirstIn).isDefined }
      .flatMap { e =>
        val children = e.children.toSeq
        for {
          station <- e.textNodes.toSeq.lift(0).map(_.text)
          date <- children.lift(2).map(_.text).flatMap {
            case rxSchedule(hours, minutes) => Some(LocalTime.of(hours.toInt, minutes.toInt))
            case _ => None
          }
          line <- children.lift(0).map(_.attr("alt")).flatMap {
            case rxSelectorFromParserLine(line) => Some(line)
            case _ => None
          }
        } yield (station, date, line)
      }
  }

  val rxSelectorTo = """à""".r
  val selectorTo = Selector[(String, LocalTime)] { el =>
    import scala.collection.JavaConversions._
    Option(el)
      .filter { _.classNames.contains("bg3") }
      .filter { _.children.toSeq.lift(1).map(_.text).flatMap(rxSelectorTo.findFirstIn).isDefined }
      .flatMap { e =>
        for {
          station <- e.textNodes.toSeq.lift(0).map(_.text)
          date <- e.children.toSeq.lift(2).map(_.text).flatMap {
            case rxSchedule(hours, minutes) => Some(LocalTime.of(hours.toInt, minutes.toInt))
            case _ => None
          }
        } yield (station, date)
      }
  }

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
    def addFrom(station: String, date: LocalTime, line: String) = copy(from = Some(station), startAt = Some(date), line = Some(line))
    def addTo(station: String, date: LocalTime) = copy(to = Some(station), finishAt = Some(date))
  }
  object ParsingCorrespondance {
    def empty = ParsingCorrespondance()
  }

  sealed trait ParsingResult { def value: json.JsValue }
  case class ParsingOk(value: json.JsValue) extends ParsingResult
  case class ParsingError(msg: Option[String]) extends ParsingResult {
    def value = Json.obj("error" -> (msg.getOrElse("error occured during parsing"): String))
  }
  case object ParsingInvalid extends ParsingResult {
    val value = Json.obj("error" -> "Can't find infos")
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
    def addFrom(station: String, date: LocalTime, line: String) = {
      correspondances match {
        case c :: cs => copy(correspondances = c.addFrom(station, date, line) :: cs)
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

    def isDefined = {
      startedAt.isDefined &&
      finishedAt.isDefined &&
      firstWalkDuration.isDefined &&
      ! correspondances.isEmpty
    }

    def toResult: ParsingResult = {
      if (isDefined) ParsingOk(Json.obj(
        "itineraire" -> Json.obj(
          "nb_correspondances" -> correspondances.size,
          "duree_total" -> finishedAt.flatMap { f => startedAt.map { s => f - s } },
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
      ))
      else ParsingInvalid
    }
  }

  object ParsingState {
    def empty: ParsingState = ParsingState()
  }

  def parseDocument(doc: Document): ParsingResult = {
    import scala.collection.JavaConversions._
    Option(doc.select(".error").first) match {
      case Some(e) => ParsingError(e.textNodes.toSeq.lift(0).map(_.text))
      case _ =>
        doc.select(".bg1,.bg2,.bg3").toSeq.foldLeft(ParsingState.empty) {
          case (s, _) if s.isFinished => s
          case (s, selectorStart(date)) => println(s"start = $date"); s.started(date)
          case (s, selectorFinish(date)) => println(s"finish = $date"); s.finished(date)
          case (s, selectorWalk(station)) => println(s"walk = $station"); s.addWalk(station)
          case (s, selectorDuration1(duration)) => println(s"duration1 = $duration"); s.addDuration1(duration)
          case (s, selectorDirection(direction)) => println(s"direction = $direction"); s.addDirection(direction)
          case (s, selectorFrom((station, date, line))) => println(s"de = $station at $date on $line"); s.addFrom(station, date, line)
          case (s, selectorTo((station, date))) => println(s"à = $station at $date"); s.addTo(station, date)
          case (s, selectorDuration3(duration)) => println(s"duration3 = $duration"); s  // osef
          case (s, selectorCorrespondance(correspondance)) => println(s"correspondance = $correspondance"); s  // osef
          case (s, e) => println(s"unknwon = $e"); s
        }.toResult
    }
  }

  type HTMLBody = String

  def call(query: Query)(implicit app: Application): Future[HTMLBody] = {
    ws.WS
     .url(query.url)
     .withQueryString(query.toQueryString: _*)
     .withHeaders("User-Agent" -> "super-user")
     .get()
     .flatMap {
       case resp if resp.status == 200 => Future.successful(resp.body: HTMLBody)
       case resp => Future.failed(UnexpectedResponse(resp))
     }
  }

  def parse(body: HTMLBody, url: String): ParsingResult = {
    val doc: Document = Parser.parse(body, url)
    parseDocument(doc)
  }

}
