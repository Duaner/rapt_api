package components

case class UnexpectedResponse(status: Int, content: String) extends Exception(
  s"Unexpected WS response: ($status) $content"
)
object UnexpectedResponse {
  def apply(resp: play.api.libs.ws.WSResponse): UnexpectedResponse = UnexpectedResponse(resp.status, resp.body)
}
