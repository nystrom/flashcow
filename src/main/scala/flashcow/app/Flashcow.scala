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

object Flashcow {

  // This is a very simple Scala servlet.
  // If you rename this class, you must also change the file
  // src/main/webapp/WEB-INF/web.xml to use the new name.
  class FlashServlet extends WebApp with FlashcowView {
    import FlashcowModel._

    object currentLearningSession extends SessionVar[LearningSession](null)

    def sendCurrentItem = {
      val session = currentLearningSession()

      session.firstItem match {
        case Some(item) =>
          reply(Ok + Json(jsonItem(item)) + Commit)
        case None =>
          reply(NotFound)
      }
    }

    def sendCurrentSession = {
      val session = currentLearningSession()
      reply(Ok + HTML(renderSession(session)) + Commit)
    }

    def updateSessionTime = {
      val ts = new java.sql.Timestamp(System.currentTimeMillis)
      val session = currentLearningSession()
      session.lastAccessed = ts
      Schema.sessions.update(session)
    }

    def populate(name: String, file: String, engLang: String = "en", itaLang: String = "it") = {
      // lazy --> will only add a session if a card gets added
      lazy val ts = new java.sql.Timestamp(System.currentTimeMillis)
      lazy val session = Schema.sessions.insert(new LearningSession(name, ts, ts))

      import scala.io.Source
      import scala.io.Codec
      import scala.collection.mutable.ListBuffer
      import scala.util.matching.Regex

      val s = Source.fromFile(file)

      val pat1 = "(.*),\\s*(.*)".r
      val pat3 = "(.*)\\|\\s*(.*)".r
      val pat2 = "(.*)\\|(.*)\\|(.*)\\|(.*)".r
      val patc = "^\\s+--".r

      var i = 0

      for (line <- s.getLines) {
        val p = line match {
          case patc() => null
          case pat2(fst, snd, thd, fth) => (fst, snd, thd.toInt, fth.toDouble)
          case pat3(fst, snd) => (fst, snd, 6, 2.0)
          case pat1(fst, snd) => (fst, snd, 6, 2.0)
          case _ => null
        }

        // println(line)
        // println("--- " + p)

        p match {
          case null =>
          case (eng, ita, interval, ef) =>
              def simpleFix(word: String): String = {
                val brackets = "\\[.*?\\]".r
                val spaces = "\\s+".r
                implicit def s2bind(s: String) = new {
                  def |>(f: String => String): String = f(s)
                }
                word |> {
                  brackets.replaceAllIn(_, "")
                } |> {
                  spaces.replaceAllIn(_, " ")
                } |> {
                  _.trim
                }
              }
              def fixToList(word: String): Seq[String] = {
                val pat1 = "\\b(.*\\S)([oaie](?:/[oaie])+)\\b".r
                val pat2 = "\\bthe\\s+(\\S+)/the (\\S+)".r
                val pat3 = "\\bthe\\s+(\\S+)/(\\S+)".r
                val pat4 = "\\bil/la\\s+(\\S+)".r
                val pat5 = "\\blo/la\\s+(\\S+)".r
                val pat6 = "\\bi/le\\s+(\\S+)".r
                simpleFix(word) match {
                  case pat1(prefix, suffix) =>
                    (suffix split "/") map { w => prefix + w }
                  case pat2(fst, snd) =>
                    List("the " + fst, "the " + snd)
                  case pat3(fst, snd) =>
                    List("the " + fst, "the " + snd)
                  case pat4(w) =>
                    List("il " + w, "la " + w)
                  case pat5(w) =>
                    List("lo " + w, "la " + w)
                  case pat6(w) =>
                    List("i " + w, "le " + w)
                  case w =>
                    (w split "\\s*/\\s*") flatMap {
                       _ split "\\s+-\\s+"
                    } filter (_ != "")
                }
              }
              def fix(word: String) = fixToList(word).mkString("", "... ", ".")

            import _root_.java.net.{URLDecoder, URLEncoder}
            val engAudioURL = "http://translate.google.com/translate_tts?tl=" + engLang + "&q=" + URLEncoder.encode(fix(eng), "UTF-8")
            val itaAudioURL = "http://translate.google.com/translate_tts?tl=" + itaLang + "&q=" + URLEncoder.encode(fix(ita), "UTF-8")
            val xml = <div>
                        <div class="row-fluid"><h1><span id="text"></span></h1></div>
                        <div class="row-fluid"><a id="link" class="btn btn-mini">Audio</a></div>
                      </div>

            if (eng.length < 256 && ita.length < 256) {
              try {
                val card = Schema.cards.insert(new Card(eng, ita, engAudioURL, itaAudioURL, xml.toString))
                val tag = Schema.tags.insert(new Tag(name, card.id))
                Schema.items.insert(new LearningItem(session.id, card.id, ef, interval, i))
                i += 1
              }
              catch {
                case ex: Exception =>
              }
            }
          }
      }
    }

    override def database = Some("cards")

    override def initializeApp = {
      System.setProperty("actors.enableForkJoin", "false")

      // Recreate the database
      txn {
        Schema.create
        Schema.prefTable.insert(new Preferences(defaultInterval = 6, minInterval = 6, defaultEF = 2.0, minEF = 1.3))
      }

      txn {
        val user = Schema.users.insert(new User("admin", "OBF:1u2a1toa1w8v1tok1u30"))
        val role = Schema.roles.insert(new Role("User"))
        Schema.userroles.insert(new UserRole(user.id, role.id))
        /*
        OBF:1u2a1toa1w8v1tok1u30
        MD5:21232f297a57a5a743894a0e4a801fc3
        CRYPT:adpexzg3FUZAk
        */
      }


      import java.io.File
      val dirName = "/Users/nystrom/Dropbox/Elements/Italian/done"
      val dir = new File(dirName)

      if (dir.exists) {
        for (file <- dir.listFiles) {
          val name = ".*/tag-([^/]+).dat".r
          file.getPath match {
            case name(tag) =>
              txn {
                try {
                  populate(tag, file.getPath)
                }
                catch {
                  case ex: java.io.IOException =>
                }
              }
            case _ =>
          }
        }
      }
    }

    // Default route -- look for a file
    route {
      case Path(p @ _*) =>
        reply(StaticFile(p))
    }

    route {
      case Path() =>
        reply(Ok + HTML(mainPage) + Commit)
    }

    route {
      case Path("edit") =>
        reply(Ok + HTML(retagPage) + Commit)
    }

    route {
      case Path("card") and card(cardId) =>
        reply(Ok + HTML(cardPage(cardId)) + Commit)
    }

    val card = param[Long]("card")
    val item = param[Long]("item")
    val tag = param[String]("tag")
    val session = param[Long]("session")
    val score = param[Int]("score")
    val tags = param[String]("tags")
    val front = param[String]("front")
    val back = param[String]("back")

    route {
      case Path("tag") and tag(tag) =>
        reply(Ok + HTML(tagPage(tag)) + Commit)
    }

    route {
      case Path("start") and tag(tag) =>
        val session = newSession(List(tag))
        currentLearningSession() = session
        reply(Ok + HTML(template("/session")) + Commit)
    }

    route {
      case Path("continue") and session(sessionId) =>
        val session = LearningSession(sessionId)
        currentLearningSession() = session
        reply(Ok + HTML(template("/session")) + Commit)
    }

    route {
      case Path("bury") and item(item) =>
        buryCard(item)
        reply(Ok + Commit)
    }

    route {
      case Path("next") and item(item) and score(q) =>
        nextCard(item, q)
        reply(Ok + Commit)
    }

    route {
      case Path("delete") and card(card) =>
        deleteCard(card)
        reply(Ok + Commit)
    }

    route {
      case Path("save") and card(card) and front(front) and back(back) and tags(tags) =>
        saveCard(currentLearningSession(), card, front, back, tags)
        reply(Ok + Commit)
    }

    route {
      case Path("dump") =>
        sendCurrentSession
    }

    route {
      case Path("poll") =>
        println("poll")
        try {
          updateSessionTime
          sendCurrentItem
        }
        catch {
          case ex: Exception =>
            ex.printStackTrace
            throw ex
        }
        finally {
          println("done with poll")
        }
    }

    route {
      case Path(path) if path.startsWith("login.html;") =>
        reply(StaticFile("login.html"))
      case Path(path) if path.startsWith("loginError.html;") =>
        reply(StaticFile("loginError.html"))
    }

    val id = param[String]("id")

    route {
      case Path("flashcow", "lazy") and id(id) =>
        reply(LazyTemplate(id))
    }
  }
}
