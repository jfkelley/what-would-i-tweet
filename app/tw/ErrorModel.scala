package tw

// Model that just throws an error instead of doing anything
class ErrorModel(e: Error) extends TweetModel {
  def this(s: String) = this(new Error(s))
  override def generateTweet(user: String): Nothing = throw e
}