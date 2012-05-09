package flashcow.app

import javax.servlet.http._
import javax.servlet.AsyncContext
import javax.servlet.ServletConfig
import scala.xml._

import org.squeryl.SessionFactory
import org.squeryl.Session
import org.squeryl.adapters.H2Adapter
import org.squeryl.PrimitiveTypeMode._

import flashcow.core._

trait FlashcowView extends View {
  self: Flashcow =>

  import FlashcowModel._

  def renderSession(session: LearningSession): NodeSeq = {
    <ul>
      {
        for (item <- session.items) yield {
          <li>
            {item.rank} {item.card.frontText} &mdash; {item.card.backText} {item.ef} {item.interval}
          </li>
        }
      }
    </ul>
  }

  def jsonItem(item: LearningItem): Map[Symbol, Any] = {
    val card = item.card

    val intervals: Seq[Int] = (0 to 10) map { i => nextEFAndInterval(item, i, false) } map { _._2 }

    val m = Map(
      'rank -> item.rank,
      'itemId -> item.id,
      'cardId -> card.id,
      'template -> card.template,
      'frontText -> card.frontText,
      'backText -> card.backText,
      'frontLink -> card.frontLink,
      'backLink -> card.backLink,
      'tags -> card.tags.map(_.name).toList.mkString(" "),
      'ef -> "%.2f".format(item.ef),
      'interval -> intervals
    )

    m
  }

  def renderItem(item: LearningItem): NodeSeq = {
    val card = item.card
    <div class="card">
      <div class="rank">{item.rank}</div>
      <div class="word front">{card.frontText}</div>
      <div class="word back">{card.backText}</div>
      <div class="audio front"></div>
      <div class="audio back"></div>
      <div class="ef">{"%.2f".format(item.ef)}</div>
      <div class="interval">{item.interval}</div>
    </div>
  }


  def renderCardSummary(card: Card): NodeSeq = {
    <span>
      <a href={"/card?card=" + card.id}>
        <span class="cardsummary front">{card.frontText}</span> &mdash; <span class="cardsummary front">{card.backText}</span>
      </a>
    </span>
  }

  def renderAllCardsWithTag(tag: String): NodeSeq = {
    <div class="allcards">
      <ul class="cardlist">
        {for (card <- Schema.cardsWithTag(tag)) yield <li class="carditem">{renderCardSummary(card)}</li>}
      </ul>
    </div>
  }

  def tagPage(tag: String): NodeSeq = {
    template("/wrapper", "title" -> tag, "content" -> 
      <div>
        <h1>{tag}</h1>
        {renderAllCardsWithTag(tag)}
      </div>
    )
  }

  def retagPage: NodeSeq = {
    template("/retag",
      "title" -> "Flash Cow",
      "cards" -> from(Schema.cards)(s => select(s)).toList,
      "card.front" -> pf[Card] { case (c: Card) => c.frontText },
      "card.back" -> pf[Card] { case (c: Card) => c.backText },
      "card.tags" -> pf[Card] { case (c: Card) => c.tags.map(_.name).toList.mkString(" ") })
  }

  def cardPage(cardId: Long): NodeSeq = {
    val card = Card(cardId)
    template("/card",
      "title" -> (card.frontText + " - " + card.backText),
      "front" -> card.frontText,
      "back" -> card.backText)
  }

  // def pf[A](f: Function1[A, Any]): PartialFunction[Any, Any] = {
    // case x: A if f.isDefinedAt(x) => f(x)
  // }
  def pf[A](f: PartialFunction[A, Any]): PartialFunction[Any, Any] = {
    case x: A if f.isDefinedAt(x) => f(x)
  }

  def ago(ts: java.sql.Timestamp) = {
    import com.twitter.util.Time
    import com.twitter.util.Time._
    import com.twitter.conversions.time._

    val t: Time = Time(ts)
    val since = now - t
    val days = since.inDays
    val hours = (since - days.days).inHours
    val minutes = (since - hours.hours).inMinutes
    val seconds = (since - minutes.minutes).inSeconds

    // println("days " + days)
    // println("hours " + hours)
    // println("minutes " + minutes)
    // println("seconds " + seconds)

    if (days > 1)
      days + " days ago"
    else if (days == 1 && hours > 1)
      days + " day " + hours + " hours ago"
    else if (days == 1 && hours == 1)
      days + " day 1 hour ago"
    else if (days == 1)
      days + " day ago"
    else if (hours > 1)
      hours + " hours ago"
    else if (hours == 1 && minutes > 1)
      hours + " hour " + minutes + " minutes ago"
    else if (hours == 1 && minutes == 1)
      hours + " hour 1 minute ago"
    else if (hours == 1)
      hours + " hour ago"
    else if (minutes > 1)
      minutes + " minutes ago"
    else if (minutes == 1)
      "1 minute ago"
    else
      "less than 1 minute ago"
  }

  def mainPage: NodeSeq = {
    template("/main",
      "title" -> "Flash Cow",
      "sessions" -> Schema.sessionList,
      "tags" -> Schema.tagList,
      "session.name" -> pf[LearningSession] { case (s: LearningSession) => s.name },
      "session.started" -> pf[LearningSession] { case (s: LearningSession) => ago(s.created) },
      "session.lastAccessed" -> pf[LearningSession] { case (s: LearningSession) => ago(s.lastAccessed) },
      "session.continue" -> pf[LearningSession] { case (s: LearningSession) => <a href={"/continue?session=" + s.id}><bind name="session.name"/></a> },
      "tag.name" -> pf[String] { case (d: String) => d },
      "tag.view" -> pf[String] { case (d: String) => <a href={"/tag?tag=" + d}>{d}</a> },
      "tag.new" -> pf[String] { case (d: String) => <a href={"/start?tag=" + d}>Start new session</a> })
  }
}

