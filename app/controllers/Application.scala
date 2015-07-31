package controllers

import java.time.LocalTime
import play.api.Play.current
import play.api._
import play.api.data.Form
import play.api.data.Forms.{ mapping, nonEmptyText, boolean, number }
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
  ) extends Scraper.Query {
    def url = ItineraryQuery.url
    def resolve: Future[Either[json.JsValue, ItineraryQuery]] = {
      for {
        typeName1 <- {
          if (type1 == "latlng") findLatlng(name1).map(_.right.map("adress" -> _))
          else Future.successful(Right(type1 -> name1))
        }
        typeName2 <- {
          if (type2 == "latlng") findLatlng(name2).map(_.right.map("adress" -> _))
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

  def callAndParse(query: Scraper.Query): Future[Result] = {
    Scraper.call(query).map { body =>
      (Scraper.parse(body, query.url) match {
        case Scraper.ParsingOk(value) => Ok(value)
        case err => InternalServerError(err.value)
      }).withHeaders(
        "Content-Type" -> "text/json; charset=utf-8",
        "Access-Control-Allow-Origin" -> "*"
      )
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
      reseau = "all",
      traveltype = "plus_rapide",
      datestart = true,
      datehour = 12,
      dateminute = 10
    ))
  }

}
