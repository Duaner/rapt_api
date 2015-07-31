package controllers

import java.time.LocalTime
import play.api.Play.current
import play.api._
import play.api.data.Form
import play.api.data.Forms.{ mapping, nonEmptyText, boolean, number, optional }
import play.api.libs.json
import play.api.libs.json.Json
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import components.{ Geodecoder, Scraper }

class Application extends Controller {

  def findLatlng(latlng: String): Future[Either[json.JsValue, String]] = {
    Geodecoder.searchAddress(latlng).map {
      case Some(address) => Right(address)
      case None => Left(Json.obj("error" -> s"No address found at coordinates $latlng"))
    }
  }

  case class SimpleQuery(
    latlng: String,
    station: String
  ) {
    def toItineraryQuery = ItineraryQuery(
      type1 = "latlng",
      name1 = latlng,
      type2 = "station",
      name2 = station
    )
  }
  val formSimpleQuery = Form(mapping(
    "latlng" -> nonEmptyText,
    "station" -> nonEmptyText
  )(SimpleQuery.apply)(SimpleQuery.unapply))

  case class ItineraryQuery(
    type1: String,
    name1: String,
    type2: String,
    name2: String,
    reseau: Option[String] = None,
    traveltype: Option[String] = None,
    datestart: Option[Boolean] = None,
    datehour: Option[Int] = None,
    dateminute: Option[Int] = None
  ) extends Scraper.Query {
    def url = ItineraryQuery.url
    def resolve: Future[Either[json.JsValue, ItineraryQuery]] = {
      for {
        typeName1 <- {
          if (type1 == "latlng") findLatlng(name1).map(_.right.map("adresse" -> _))
          else Future.successful(Right(type1 -> name1))
        }
        typeName2 <- {
          if (type2 == "latlng") findLatlng(name2).map(_.right.map("adresse" -> _))
          else Future.successful(Right(type2 -> name2))
        }
      } yield {
        typeName1.right.flatMap { case (type1, name1) =>
          typeName2.right.map { case (type2, name2) =>
            copy(type1 = type1, name1 = name1, type2 = type2, name2 = name2)
          }
        }
      }
    }
    def toQueryString = Seq[(String, Option[String])](
      "type1" -> Some(type1),
      "name1" -> Some(name1),
      "type2" -> Some(type2),
      "name2" -> Some(name2),
      "reseau" -> reseau,
      "traveltype" -> traveltype,
      "datestart" -> datestart.map(_.toString),
      "datehour" -> datehour.map(_.toString),
      "dateminute" -> dateminute.map(_.toString)
    ).collect { case (k, Some(v)) => (k, v) }
  }
  object ItineraryQuery {
    val url = "http://wap.ratp.fr/siv/itinerary-list"
  }

  val formItinerateQuery = Form(mapping(
    "type1" -> nonEmptyText,
    "name1" -> nonEmptyText,
    "type2" -> nonEmptyText,
    "name2" -> nonEmptyText,
    "reseau" -> optional(nonEmptyText),
    "traveltype" -> optional(nonEmptyText),
    "datestart" -> optional(boolean),
    "datehour" -> optional(number(min=0, max=23)),
    "dateminute" -> optional(number(min=0, max=59))
  )(ItineraryQuery.apply)(ItineraryQuery.unapply))

  def callAndParse(query: ItineraryQuery): Future[Result] = {
    query.resolve.flatMap {
      case Right(query) =>
        Scraper.call(query).map { body =>
          (Scraper.parse(body, query.url) match {
            case Scraper.ParsingOk(value) => Ok(value)
            case err => InternalServerError(err.value)
          })
        }
      case Left(err) => Future.successful(BadRequest(err))
    }.map { res =>
      res.withHeaders(
        "Content-Type" -> "text/json; charset=utf-8",
        "Access-Control-Allow-Origin" -> "*"
      )
    }
  }

  def index = Action.async { implicit req =>
    import play.api.i18n.Messages.Implicits._
    formSimpleQuery.bindFromRequest.fold(
      err => {
        formItinerateQuery.bindFromRequest.fold(
          _ => Future.successful {
            BadRequest(Json.obj("error" -> "bad request", "errors" -> err.errorsAsJson))
          },
          callAndParse
        )
      },
      q => callAndParse(q.toItineraryQuery)
    )
  }

  def search(latlng: String) = Action.async {
    findLatlng(latlng).map {
      case Right(address) => Ok(Json.obj("address" -> address))
      case Left(err) => NotFound(err)
    }
  }

  def test = Action.async {
    callAndParse(ItineraryQuery(
      type1 = "station",
      name1 = "Danube",
      type2 = "station",
      name2 = "Pernety",
      reseau = Some("all"),
      traveltype = Some("plus_rapide"),
      datestart = Some(true),
      datehour = Some(12),
      dateminute = Some(10)
    ))
  }

}
