package tw

// Uses a regular expression. Returns all tokens matching that expression.
object SimpleTweetTokenizer extends TweetTokenizer {
  val regularWord = """[a-zA-Z](['\-]?[a-zA-Z])*"""
  val number = """#?\-?$?[0-9]+(\.[0-9]+)?"""
  val hashtag = """#[a-zA-Z0-9]+"""
  val atMention = """@[a-zA-Z0-9_]+"""
  val url = """(http://)?[a-zA-Z0-9\-]+(\.[a-zA-Z0-9\-]+)+(/[#a-zA-Z0-9\-\.]+)*(\?([a-zA-Z0-9\.\-!%](=[a-zA-Z0-9\.\-!%])?)(&[a-zA-Z0-9\.\-!%](=[a-zA-Z0-9\.\-!%])?)*)?"""
  val token = s"(($url)|($regularWord)|($number)|($hashtag)|($atMention))" + """[\.\?!,;:]?"""
  val tokenRegex = token.r // Ok, maybe it's not so simple...
  
  def tokenize(tweet: Tweet): List[String] = {
    tokenRegex.findAllIn(tweet.text).toList
  }
}