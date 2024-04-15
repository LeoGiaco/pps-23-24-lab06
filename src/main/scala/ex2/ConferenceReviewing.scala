package ex2

enum Question:
    case RELEVANCE
    case SIGNIFICANCE
    case CONFIDENCE
    case FINAL

trait ConferenceReviewing:

  /**
	 * @param article
	 * @param scores
	 * loads a review for the specified article, with complete scores as a map
	 */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
	
	/**
	 * @param article
	 * @param relevance
	 * @param significance
	 * @param confidence
	 * @param fin
	 * loads a review for the specified article, with the 4 explicit scores
	 */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
	
	/**
	 * @param article
	 * @param question
	 * @return the scores given to the specified article and specified question, as an (ascending-ordered) list 
	 */
  def orderedScores(article: Int, question: Question): Seq[Int]
	
	/**
	 * @param article
	 * @return the average score to question FINAL taken by the specified article
	 */
  def averageFinalScore(article: Int): Double
	
	/**
	 * An article is considered accept if its averageFinalScore (not weighted) is > 5, 
	 * and at least one RELEVANCE score that is >= 8.
	 * @return the set of accepted articles
	 */
  def acceptedArticles(): Set[Int]
	
	
	/**
	 * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
	 */
  def sortedAcceptedArticles(): Seq[(Int, Double)]
	
	/**
	 * @return a map from articles to their average "weighted final score", namely,
	 * the average value of CONFIDENCE*FINAL/10  
	 * Note: this method is optional in this exam
	 */
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  import scala.collection.mutable.{Map => MutableMap}
  import scala.collection.mutable.{Buffer => MutableSeq}

  private class ConferenceReviewingImpl extends ConferenceReviewing:
    private val articles: MutableMap[Int, MutableSeq[Map[Question, Int]]] = MutableMap()

    def loadReview(article: Int, scores: Map[Question, Int]): Unit = articles.get(article) match
      case Some(seq) => seq += scores
      case None      => articles.addOne(article, MutableSeq(scores))

    def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = 
      loadReview(article, Map((Question.RELEVANCE, relevance), (Question.SIGNIFICANCE, significance), (Question.CONFIDENCE, confidence), (Question.FINAL, fin)))
    
    def orderedScores(article: Int, question: Question): Seq[Int] = articles.get(article) match
      case Some(art) => lazyGetVotesToQuestion(art)(question).sortBy(v => v).toList
      case None      => Seq()
    
    private def lazyGetVotesToQuestion(article: MutableSeq[Map[Question, Int]])(question: Question): LazyList[Int] = LazyList.from(article).map(m => m.getOrElse(question, 0))

    def averageFinalScore(article: Int): Double = articles.get(article) match
      case Some(art) => lazyGetVotesToQuestion(art)(Question.FINAL).sum.doubleValue() / art.length
      case None      => -1
    
    def acceptedArticles(): Set[Int] = articles.collect:
      case (art, votes) if averageFinalScore(art) > 5 && lazyGetVotesToQuestion(votes)(Question.RELEVANCE).find(rel => rel >= 8).isDefined => art
    .toSet

    def sortedAcceptedArticles(): Seq[(Int, Double)] = acceptedArticles().map(art => (art, averageFinalScore(art))).toSeq.sortBy(v => v._2)

    def averageWeightedFinalScoreMap(): Map[Int, Double] = 
      articles.map((art, votes) => (art, LazyList.from(votes).map(m =>
          val conf = m.getOrElse(Question.CONFIDENCE, 0)
          val fin = m.getOrElse(Question.FINAL, 0)
          conf * fin / 10
        ).sum.doubleValue() / votes.length)).toMap

  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

object TestConferenceReviewing extends App:
  import ConferenceReviewing.*

  val cr = ConferenceReviewing()
  cr.loadReview(1, 8, 8, 6, 8); // 4.8 è il voto finale pesato
  cr.loadReview(1, 9, 9, 6, 9); // 5.4 
  cr.loadReview(2, 9, 9, 10, 9); // 9.0
  cr.loadReview(2, 4, 6, 10, 6); // 6.0
  cr.loadReview(3, 3, 3, 3, 3); // 0.9
  cr.loadReview(3, 4, 4, 4, 4); // 1.6
  cr.loadReview(4, 6, 6, 6, 6); // 3.6
  cr.loadReview(4, 7, 7, 8, 7); // 5.6
  var map = Map[Question, Int]()
  map = map.updated(Question.RELEVANCE, 8);
  map = map.updated(Question.SIGNIFICANCE, 8);
  map = map.updated(Question.CONFIDENCE, 7); // 5.6
  map = map.updated(Question.FINAL, 8);
  cr.loadReview(4, map);
  cr.loadReview(5, 6, 6, 6, 10); // 6.0
  cr.loadReview(5, 7, 7, 7, 10); // 7.0

  println:
    cr.orderedScores(2, Question.RELEVANCE)
  println:
    cr.orderedScores(4, Question.CONFIDENCE)
  println:
    cr.orderedScores(5, Question.FINAL)
