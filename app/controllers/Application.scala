package controllers

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.sql.Connection
import org.apache.commons.codec.binary.Base64
import anorm._
import play.api._
import play.api.Play.current
import play.api.db.DB
import play.api.mvc._
import twitter4j.Twitter
import twitter4j.TwitterFactory
import twitter4j.auth.AccessToken
import twitter4j.auth.RequestToken
import twitter4j.Paging
import tw.PublicAPIGetter
import tw.TokenGraphModel
import tw.SimpleTweetTokenizer
import tw.TweetModel
import play.api.cache.Cache
import tw.ErrorModel

object Application extends Controller {
  
  val twitterFactory = new TwitterFactory
  val userIdSessionKey = "userId"
  val requestTokenSessionKey = "requestToken"

  // Main application entry point
  def index = Action { implicit request =>
    session.get(userIdSessionKey).map(_.toLong) match {
      case Some(id) => handleLoggedIn(request, id)
      case None => handleNotLoggedIn(request)
    }
  }
  
  /*
   * Given that the user has an ID, attempt to load their accessToken from the database. If
   * able to load it, send them to the tweet generating page. Otherwise, clear their userId
   * and have them log in again in order to save the accessToken.
   */
  def handleLoggedIn(request: Request[AnyContent], userId: Long)(implicit session: Session) = {
    val accessToken = DB.withConnection { implicit c => getAccessToken(userId) }
    accessToken match {
      case Some(token) => Redirect(routes.Application.generateTweet("_"))
      case None => Redirect(routes.Application.index).withSession(session - userIdSessionKey)
    }
  }
  
  /*
   * If a user is not logged in, they go through twitter authentication. We redirect
   * them to twitter's auth page, with a callback to this same page. After they click
   * through the twitter page, they are sent back to that callback with an "oauth_verifier"
   * parameter. We handle both cases here.
   */
  def handleNotLoggedIn(request: Request[AnyContent])(implicit session: Session) = {
    request.getQueryString("oauth_verifier") match {
      case None => redirectToTwitterAuth(request) // first hit
      case Some(verifier) => { // second hit (via callback)
        session.get(requestTokenSessionKey) match {
          case Some(requestTokenSerialized) => displayLoggedInPage(requestTokenSerialized, verifier)
          case _ => redirectToTwitterAuth(request)
        }
      }
    }
  }
  
  // Send the user to authenticate with twitter
  def redirectToTwitterAuth(request: Request[AnyContent]) = {
    val twitter = twitterFactory.getInstance()
    val callbackUri = request.uri
    val requestToken = twitter.getOAuthRequestToken("http://" + request.host + "/");
    val authUrl = requestToken.getAuthenticationURL
    Redirect(authUrl).withSession((requestTokenSessionKey -> serialize(requestToken)))
  }
  
  // After the user logs in on twitter, save their credentials, and send them to generate a tweet
  def displayLoggedInPage(requestTokenSerialized: String, oauthVerifier: String)(implicit session: Session) = {
    val twitter = twitterFactory.getInstance()
    val requestToken = deserialize[RequestToken](requestTokenSerialized)
    val accessToken = twitter.getOAuthAccessToken(requestToken, oauthVerifier);
    saveAccessToken(accessToken)
    twitter.setOAuthAccessToken(accessToken)
    Redirect(routes.Application.generateTweet("_")).withSession(session + (userIdSessionKey -> accessToken.getUserId.toString) - requestTokenSessionKey)
  }
  
  // Actually generate a tweet! If the screenName is _, replace it with the name of the logged in user
  def generateTweet(screenName: String) = Action { implicit request =>
    getLoggedInTwitter match {
      case Some(twitter) => {
        val userName = if (screenName == "_") twitter.getScreenName() else screenName
        val tweetModel = getTweetModel(twitter, userName)
        Ok(userName + " would tweet:\n" + tweetModel.generateTweet(userName))
      }
      case None => Redirect(routes.Application.index).withSession(session - userIdSessionKey)
    }
  }
  
  // Getting all tweets from a given user is expensive, so TweetModels are cached for 1 hour
  def getTweetModel(twitter: Twitter, user: String): TweetModel = {
    Cache.getOrElse("tm:" + user, 60*60){
      val tweetsOrError = new PublicAPIGetter(twitter, user).getTweets
      tweetsOrError match {
        case Left(Nil) => new ErrorModel("No Tweets")
        case Left(tweets) => new TokenGraphModel(SimpleTweetTokenizer, tweets)
        case Right(error) => new ErrorModel(error)
      }
    }
  }
  
  // Get a Twitter object authenticated with the currently logged-in user
  def getLoggedInTwitter(implicit session: Session): Option[Twitter] = {
    session.get(userIdSessionKey).map(_.toLong).flatMap(userId =>
      DB.withConnection { implicit c => getAccessToken(userId) }.map { accessToken =>
        val twitter = twitterFactory.getInstance
        twitter.setOAuthAccessToken(accessToken)
        twitter
      }
    )
  }
  
  /*
   * Save an access token to the database. The access_tokens table has three columns:
   * user_id, token, and secret, where user_id is the primary key.
   * 
   * If a token already exists for a user, do an update. If not, do an instert.
   */
  def saveAccessToken(accessToken: AccessToken) = DB.withConnection{ implicit conn =>
    val (userId, token, secret) = (accessToken.getUserId, accessToken.getToken, accessToken.getTokenSecret)
    getAccessToken(accessToken.getUserId) match {
      case Some(_) => {
        val result = SQL("update access_tokens set token = {token}, secret = {secret} where user_id = {userId};")
        .on(("token" -> token), ("secret" -> secret), ("userId" -> userId))
        .executeUpdate
        if (result != 1) {
          throw new Exception("Tried to update access token for user " + accessToken.getUserId + " but update changed " + result + " rows.")
        }
      }
      case None => {
        val result = SQL("insert into access_tokens(user_id, token, secret) values ({userId}, {token}, {secret});")
        .on(("token" -> token),("secret" -> secret),("userId" -> userId))
        .executeInsert()
        result match {
          case Some(n) if n == userId =>
          case Some(n) => throw new Exception("Tried to insert access token for user " + accessToken.getUserId + " but the key used was " + n + ".")
          case None => throw new Exception("Tried to insert access token for user " + accessToken.getUserId + " but nothing was inserted.")
        }
      }
    }
  }
  
  // Retrieves an access token from the database, if one exists with the given userId
  def getAccessToken(userId: Long)(implicit c: Connection): Option[AccessToken] = {
    SQL("select token, secret from access_tokens where user_id = {id};").on("id" -> userId).singleOpt()(c).map{
      case Row(Some(token: String), Some(secret: String)) => new AccessToken(token, secret, userId)
    }
  }
  
  
  // Serialize any serializable type to a string. Uses java serialization and default encoding
  def serialize[T <% Serializable](t: T): String = {
    val bo = new ByteArrayOutputStream
    val oo = new ObjectOutputStream(bo)
    oo.writeObject(t)
    oo.close
    new String(Base64.encodeBase64(bo.toByteArray()))
  }
  
  // Deserialize from a string. Uses java serialization and default encoding
  def deserialize[T <% Serializable](s: String): T = {
    val bi = new ByteArrayInputStream(Base64.decodeBase64(s.getBytes()))
    val oi = new ObjectInputStream(bi)
    val result = oi.readObject.asInstanceOf[T]
    oi.close
    result
  }

}