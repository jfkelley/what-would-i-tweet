package tw

trait TweetGetter {
  def getTweets: Either[List[Tweet], Error]
}