package tw

import scala.util.Random

/*
 * Imagine a tweet as a directed graph from token to its following token.
 * And then a collection of tweets as a graph where the only edges between tweets
 * are cases like the following:
 * 
 * A - B - C
 *       X
 * G - B - D
 * 
 * Since both tweets have a "B" token, they share those outlinks. This model generates
 * a tweet by taking a semi-random walk across this graph. The guiding principles used for
 * this walk are:
 * 
 * 1. It's generally better to jump to a token whose preceding two tokens match the last
 *    two tokens emitted, where possible. This makes tweets more coherent.
 * 2. The exception to the above is if you've been just walking along a single tweet for
 *    too long, prefer switching when possible so an entire duplicate tweet isn't generated.
 * 3. If you have no preference, choose randomly
 *  
 * This graph is not represented explicitly. Instead, I maintain a list of lists of tokens
 * and an index to allow getting all occurrences of the same token across all tweets.
 */
class TokenGraphModel(tokenizer: TweetTokenizer, trainingTweets: List[Tweet]) extends TweetModel {
  val punctuation = List(',','.','!','.',':',';')
  
  val tokens = trainingTweets.map(tweet => Start +: splitPunctuationOff(tokenizer.tokenize(tweet)).map(Text(_)) :+ End)
  val tokenIndex = index(tokens)
  
  sealed abstract class Token
  case object Start extends Token
  case object End extends Token
  case class Text(text: String) extends Token
  
  // Like a pointer
  case class TokenRef(r: Int, c: Int) {
    def getToken = tokens(r)(c)
    def next = new TokenRef(r, c + 1)
    def prev = new TokenRef(r, c - 1)
  }
  
  def splitPunctuationOff(tokens: List[String]) = {
    val newTokens = tokens.map { token =>
      if (punctuation.contains(token.last)) {
        List(token.dropRight(1), token.takeRight(1))
      } else {
        List(token)
      }
    }
    newTokens.flatten.toList
  }
  
  def index(tokens: List[List[Token]]): Map[Token, List[TokenRef]] = {
    val withIndexes: List[(Token, TokenRef)] = tokens.zipWithIndex.map{ case (list, r) =>
      list.zipWithIndex.map{ case (token, c) =>
        (token, new TokenRef(r, c))
      }
    }.flatten
    withIndexes.foldRight(Map.empty[Token, List[TokenRef]]){ case ((token, ref), map) =>
      if (map.contains(token)) map + (token -> (ref :: map(token))) else map + (token -> List(ref))
    }
  }
  
  def getNextOptions(ref: TokenRef): List[TokenRef] = {
    ref.getToken match {
      case End => List()
      case x => tokenIndex(x).map(_.next)
    }
  }
  
  def prevMatches(ref: TokenRef, token: Token) = ref.prev.getToken == token
  
  def generateTweet(user: String) = {
    generateTweet(List((randomStartRef, false)))
  }
  
  def generateTweet(acc: List[(TokenRef, Boolean)]): String = {
    val result = makeTweet(acc)
    if (result.size > 140) {
      makeTweet(cutTweet(acc))
    } else {
      generateTweet(getNextTokenOrEnd(acc.map(_._1)) :: acc)
    }
  }
  
  def cutTweet(acc: List[(TokenRef, Boolean)]): List[(TokenRef, Boolean)] = {
    val i = acc.indexWhere(_._2)
    if (i == -1) {
      acc.tail
    } else {
      acc.drop(i + 1)
    }
  }
  
  def makeTweet(acc: List[(TokenRef, Boolean)]): String = {
    acc.foldRight(""){ case ((ref, _), str) =>
      ref.getToken match {
        case Text(s) if str.length == 0 || (s.length == 1 && punctuation.contains(s.head)) => str + s
        case Text(s) => str + " " + s
        case _ => str
      }
    }
  }
  
  implicit def listWithRandomChoice[T](list: List[T]) = new {
    def randomElement = list(Random.nextInt(list.length))
  }
  
  def randomStartRef = tokenIndex(Start).randomElement
  
  def getNextTokenOrEnd(soFar: List[TokenRef]): (TokenRef, Boolean) = {
    val withEnds = getNextToken(soFar, false)
    if (withEnds.getToken == End) {
      (getNextToken(soFar, true), true)
    } else {
      (withEnds, false)
    }
  }
  
  // Core logic of generating a tweet
  def getNextToken(soFar: List[TokenRef], filterEnds: Boolean) = {
    val matchOnes = getNextOptions(soFar.head).filter(!filterEnds || _.getToken != End)
    if (matchOnes.isEmpty) {
      getNextOptions(randomStartRef).randomElement
    } else {
      if (soFar.length >= 2) {
        val matchTwos = matchOnes.filter(ref => prevMatches(ref.prev, soFar.tail.head.getToken))
        val currentTweet = soFar.head.r
        val timeInSameTweet = soFar.takeWhile(_.r == currentTweet).size
        val (matchTwosSame, matchTwosDifferent) = matchTwos.partition(_.r == currentTweet)
        val (matchOnesSame, matchOnesDifferent) = matchOnes.partition(_.r == currentTweet)
        if (Random.nextDouble() <= timeInSameTweet / 10.0 && !(matchTwosDifferent ::: matchOnesDifferent).isEmpty) {
          if (matchTwosDifferent.isEmpty) {
            matchOnesDifferent.randomElement
          } else {
            matchTwosDifferent.randomElement
          }
        } else {
          if (matchTwos.isEmpty) {
            matchOnes.randomElement
          } else {
            matchTwos.randomElement
          }
        }
      } else {
        matchOnes.randomElement
      }
    }
  }
}