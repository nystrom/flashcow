package flashcow.app

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.KeyedEntity
import scala.xml._
import java.sql.SQLException
import java.sql.Timestamp

import flashcow.core._

package object FlashcowModel {
  object Card { def apply(id: Long) = Schema.cards.lookup(id).get }
  object LearningSession { def apply(id: Long) = Schema.sessions.lookup(id).get }
  object LearningItem { def apply(id: Long) = Schema.items.lookup(id).get }
  object Tag { def apply(id: Long) = Schema.tags.lookup(id).get }

  // New model:
  /*
  class Score(user, card, numTimesSeen, numTimesRight, created, firstSeen, lastSeen, lastEdited)
  class Session(user, name, created, started, lastSeen, algorithm)
  class Item(session, card, ef, interval, rank)

  front and back are in markup, not text
  can include images, sounds

  */

  class Score(/* val userId: Int, */ val cardId: Long, val numTimesSeen: Int,
    val numTimesCorrect: Int, val firstSeen: Timestamp, val lastSeen: Timestamp) {
    val id = 0L;
  }

  class User(val name: String, val pass: String) extends KeyedEntity[Int] {
    val id = 0
  }
  class UserRole(val userId: Int, val roleId: Int) extends KeyedEntity[Int] {
    val id = 0
  }
  class Role(val name: String) extends KeyedEntity[Int] {
    val id = 0
  }

  class Tag(val name: String, val cardId: Long) extends KeyedEntity[Long] {
    val id = 0L
    def card = Card(cardId)
  }

  class Preferences(val defaultInterval: Int, minInterval: Int, val defaultEF: Double, minEF: Double) extends KeyedEntity[Long] { 
    val id = 0L
  }

  class LearningSession(val name: String, val created: Timestamp, var lastAccessed: Timestamp) extends KeyedEntity[Long] {
    val id = 0L
    def items: List[LearningItem] = (from(Schema.items)(s => where(s.sessionId === id) select(s) orderBy(s.rank~))).toList
    def firstItem: Option[LearningItem] = {
      val minRank = from(Schema.items)(s => where(s.sessionId === id) compute(min(s.rank)))
      Some {
      from(Schema.items)(s => where((s.sessionId === id) and (s.rank in minRank)) select(s)).single
    }}
    def lastItem: Option[LearningItem] = {
      val maxRank = from(Schema.items)(s => where(s.sessionId === id) compute(max(s.rank)))
      Some {
      from(Schema.items)(s => where((s.sessionId === id) and (s.rank in maxRank)) select(s)).single
    }}
  }

  class LearningItem(val sessionId: Long, val cardId: Long, var ef: Double, var interval: Int, var rank: Int) extends KeyedEntity[Long] {
    val id = 0L
    def session = LearningSession(sessionId)
    def card = Card(cardId)
  }

  class Card(val frontText: String, val backText: String, val frontLink: String, val backLink: String, val templateHtml: String) extends KeyedEntity[Long] {
    val id = 0L

    def template: NodeSeq = XML.loadString(templateHtml)

    def tags = from(Schema.tags)(s => where(s.cardId === id) select(s) orderBy(s.name.~))
    def tagList = tags.toList
  }

  // The database schema
  object Schema extends org.squeryl.Schema {
    val cards = table[Card]
    val sessions = table[LearningSession]
    val items = table[LearningItem]
    val prefTable = table[Preferences]
    val tags = table[Tag]

    val users = table[User]
    val roles = table[Role]
    val userroles = table[UserRole]

    def prefs = from(prefTable)(s => select(s)).single

    def tagList: List[String] = from(tags)(t => select(&(t.name))).distinct.toList
    def sessionList: List[LearningSession] = (for (s <- from(sessions)(s => select(s))) yield s).toList

    def cardsWithTag(tag: String) = cardsWithTags(List(tag))
    
    def cardsWithTags(tags: List[String]): Iterable[Card] = {
      def q(t: Tag, c: Card) = tags.foldLeft[org.squeryl.dsl.ast.BinaryOperatorNodeLogicalBoolean](t.cardId === c.id) {
        case (left, tag) => left and (t.name.~ === tag)
      }
      from(Schema.tags, Schema.cards)((t,c) => where(q(t, c)) select(c) orderBy(c.frontText.~)).distinct
    }

    on(users)(user => declare(
      user.name is (unique, dbType("varchar(100)")),
      user.pass is (dbType("varchar(100)"))))
    on(userroles)(ur => declare(
      ur.userId is (indexed)))
    on(roles)(role => declare(
      role.name is (unique, dbType("varchar(100)"))))
    on(tags)(tag => declare(
      columns(tag.cardId, tag.name) are (unique),
      tag.name is (indexed)))
    on(cards)(card => declare(
      columns(card.frontText, card.backText) are (unique),
      card.templateHtml is (dbType("varchar(1024)")),
      card.frontLink is (dbType("varchar(1024)")),
      card.backLink is (dbType("varchar(1024)")),
      card.frontText is (dbType("varchar(1024)")),
      card.backText is (dbType("varchar(1024)"))
    ))
    on(items)(item => declare(columns(item.sessionId, item.rank) are (unique, indexed)))
  }


  val random = new scala.util.Random

  def nextEF(ef: Double, q: Int) = {
    if (q == 0) 1.3
    else (ef + (.1 - (5-q) * (.08 + (5-1) * .02))) max 1.3
  }

  def nextEFAndInterval(item: LearningItem, q: Int, jitter: Boolean): (Double, Int) = {
    if (jitter) {
      // jitter a bit to avoid repeating the same subsequences over and over
      val (newEF, newInterval) = nextEFAndInterval(item, q, false)
      (newEF, ( newInterval * (.98 + random.nextDouble / 10.) ).toInt max 6)
    }
    else {
      val interval = item.interval
      val newEF = nextEF(item.ef, q max 0 min 10)
      val newInterval = if (q <= 0) 6 else (interval * newEF).toInt
      (newEF, newInterval)
    }
  }

  def buryCard(itemId: Long) = {
    val item = LearningItem(itemId)
    val last = item.session.lastItem.get
    item.rank = last.rank + 1
    Schema.items.update(item)
    saveCSV(item.session)
  }

  def nextCard(itemId: Long, q: Int) = {
    val item = LearningItem(itemId)
    val ef = item.ef
    val interval = item.interval
    val (newEF, newInterval) = nextEFAndInterval(item, q, true)
 
    Schema.items.update(s =>
      where((s.sessionId === item.sessionId) and (s.rank.~ <= newInterval))
      set(s.rank := s.rank.~ - 1))

    item.ef = newEF
    item.interval = newInterval
    item.rank = newInterval
    Schema.items.update(item)

    saveCSV(item.session)
  }

  def saveCard(session: LearningSession, cardId: Long, front: String, back: String, tags: String) = {
    val sessions: List[LearningSession] = from(Schema.items)(i => where(i.id === cardId) select(i.sessionId)) map { id => LearningSession(id) } toList

    Schema.cards.update(s =>
      where(s.id === cardId)
      set(s.frontText := front, s.frontText := back))
    Schema.tags.deleteWhere(s => s.id === cardId)
    Schema.tags.insert(for (tag <- tags.split(' ').filter(_!="")) yield new Tag(tag, cardId))

    for (session <- sessions) {
      saveCSV(session)
    }
  }

  def deleteCard(cardId: Long) = {
    val sessions: List[LearningSession] = from(Schema.items)(i => where(i.id === cardId) select(i.sessionId)) map { id => LearningSession(id) } toList

    Schema.items.deleteWhere(s => s.cardId === cardId)
    Schema.tags.deleteWhere(s => s.cardId === cardId)
    Schema.cards.deleteWhere(s => s.id === cardId)

    for (session <- sessions) {
      saveCSV(session)
    }
  }

  def saveCSV(s: LearningSession) = {
    val file = "%s%sdata%stag-%s.dat".format(rootDirectory, File.separator, File.separator, s.name)
    val out = new java.io.PrintStream(new java.io.FileOutputStream(file))
    for (item <- s.items) {
      out.println("%s|%s|%d|%.2f".format(item.card.frontText, item.card.backText, item.interval, item.ef))
    }
    out.close
  }

  def newSession(tags: List[String]) = {
    val ts = new java.sql.Timestamp(System.currentTimeMillis)
    val session = Schema.sessions.insert(new LearningSession(tags.mkString(" "), ts, ts))
    val prefs = Schema.prefs

    val cards = Schema.cardsWithTags(tags)

    val count = cards.toList.length
    val ranks = Array.tabulate(count)(i => i)

    val buf = new scala.collection.mutable.ListBuffer[LearningItem]

    // shuffle
    for (j <- ranks.indices) {
        val mask = (Integer.highestOneBit(count) << 1) - 1
        var i = random.nextInt & mask
        while (i < 0 || i >= count) {
          i = random.nextInt & mask
        }
        
        val t = ranks(i)
        ranks(i) = ranks(j)
        ranks(j) = t
    }

    var rank = 0

    for (card <- cards) {
      buf += new LearningItem(session.id, card.id, prefs.defaultEF, prefs.defaultInterval, ranks(rank))
      rank += 1
    }

    Schema.items.insert(buf)

    session
  }
}
