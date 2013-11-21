package tw

import java.nio.charset.Charset
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.collection.JavaConversions._
import org.apache.commons.codec.binary.Base64
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.libs.ws.Response
import play.api.libs.ws.WS
import twitter4j.Twitter
import twitter4j.TwitterFactory
import twitter4j.auth.AccessToken
import twitter4j.TwitterException
import twitter4j.Paging
import twitter4j.Status
import twitter4j.ResponseList

// Gets all of a user's public tweets from the API, filtering retweets and messages starting with "@"
class PublicAPIGetter(twitter: Twitter, user: String) extends TweetGetter {
  def getTweets(): Either[List[Tweet], Error] = {
    try {
      val statuses = getAllTweets(twitter, user)
      Left(
        statuses
          .filter(status => !status.isRetweet && !status.getText.startsWith("RT") && !status.getText.startsWith("@"))
          .map(status => Tweet(user, status.getText))
          .toList
      )
    } catch {
      case e: TwitterException => Right(new Error(e.getErrorMessage()))
    }
  }
  
  def getAllTweets(twitter: Twitter, user: String): Iterable[Status] =
    getTweetStream(twitter, user).takeWhile(!_.isEmpty).foldLeft(List.empty[Status])((list, responseList) => list ::: responseList.toList)
  
  def getTweetStream(twitter: Twitter, user: String): Stream[ResponseList[Status]] =
    pagesStream().map(page => twitter.getUserTimeline(user, new Paging(page, 180)))
  
  def pagesStream(n: Int = 1): Stream[Int] = n #:: pagesStream(n + 1)
}