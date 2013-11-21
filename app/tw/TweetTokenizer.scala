package tw

trait TweetTokenizer {
  def tokenize(t: Tweet): List[String]
}