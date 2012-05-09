package flashcow.core

import javax.servlet.http._
import javax.servlet.AsyncContext
import javax.servlet.ServletConfig
import scala.xml._

trait Replies {
  self: WebApp =>

  trait ReplyM {
    def fill(r: HttpServletResponse): Unit
  }

  trait Reply extends ReplyM

  trait UncommittedReply extends ReplyM {
    def +(other: Reply): Reply = {
      val self = this
      new Reply {
        def fill(r: HttpServletResponse) {
          self.fill(r)
          other.fill(r)
        }
      }
    }

    def +(other: UncommittedReply): UncommittedReply = {
      val self = this
      new UncommittedReply {
        def fill(r: HttpServletResponse) {
          self.fill(r)
          other.fill(r)
        }
      }
    }

    def commit: Reply = {
      val self = this
      new Reply {
        def fill(r: HttpServletResponse) {
          self.fill(r)
        }
      }
    }
  }

  case class StringReply(msg: String) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      r.getWriter.write(msg)
    }
  }

  case class SourceReply(src: scala.io.Source) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      r.getOutputStream.write(src.map(_.toByte).toArray)
    }
  }

  val contentTypes = Map(
    "html" -> "text/html",
    "htm" -> "text/html",
    "css" -> "text/css",
    "jpg" -> "image/jpeg",
    "jpeg" -> "image/jpeg",
    "png" -> "image/png",
    "txt" -> "text/plain",
    "js" -> "text/javascript",
    "json" -> "application/json",
    "xml" -> "application/xml"
  )

  case class ContentLength(length: Int) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      r.setContentLength(length)
    }
  }

  case class ContentType(typ: String) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      r.setContentType(typ)
    }
  }

  object ContentType {
    import java.io.File
    def fromFile(file: File) = fromPath(file.getPath)
    def fromPath(name: String) = {
      val suffix = name.split('.').last
      contentTypes.get(suffix) match {
        case Some(typ) => new ContentType(typ)
        case None => new ContentType("application/x-octet-stream")
      }
    }
  }

  case class Encoding(enc: String) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      r.setCharacterEncoding(enc)
    }
  }

  case class Header(key: String, value: Any) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      value match {
        case v: java.util.Date => r.addDateHeader(key, v.getTime)
        case v: Int => r.addIntHeader(key, v)
        case v: String => r.addHeader(key, v)
        case v => r.addHeader(key, v.toString)
      }
    }
  }

  case class Redirect(loc: String) extends Reply {
    def fill(r: HttpServletResponse) {
      r.sendRedirect(loc)
    }
  }

  case class Cookie(cookie: javax.servlet.http.Cookie) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      r.addCookie(cookie)
    }
  }

  case class Locale(loc: java.util.Locale) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      r.setLocale(loc)
    }
  }

  def Json(map: Map[Symbol, Any]) = {
    def escape(v: Any) = v.toString flatMap {
      case '\"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\r' => "\\r"
      // case ch if ch >= 128 => "\\u%04x".format(ch.toInt)
      // case ch if Character.isISOControl(ch) => "\\u%04x".format(ch)
      case ch => ch.toString
    }
   
    def quote(s: String) = "\"" + s + "\""
   
    def quoteValue(v: Any): String = v match {
      case v: NodeSeq => quoteValue(v.toString)
      case xs: Seq[_] => { xs.map(x => quoteValue(x)) }.mkString("[", ", ", "]")
      case v => quote(escape(v))
    }

    val str = 
      map.toList.map {
        case (k, v) => quote(k.name) + ": " + quoteValue(v)
      }.mkString("{", ",", "}")

    ContentType("application/json") + StringReply(str) 
  }

  case class HTML(ns: NodeSeq) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      val msg = new PrettyPrinter(72, 2).formatNodes(ns)
      if (r.getContentType == null)
        r.setContentType("text/html")
      r.getWriter.write("<!DOCTYPE html>")
      r.getWriter.write(msg)
    }
  }

  class Status(status: Int) extends UncommittedReply {
    def fill(r: HttpServletResponse) {
      r.setStatus(status);
    }
  }

  class Error(status: Int) extends Reply {
    def fill(r: HttpServletResponse) {
      r.sendError(status);
    }
  }

  object Commit extends Reply {
    def fill(r: HttpServletResponse) { }
  }

  // 200
  object Ok extends Status(HttpServletResponse.SC_OK)
  // 202
  object Accepted extends Status(HttpServletResponse.SC_ACCEPTED)
  // 400
  object BadRequest extends Error(HttpServletResponse.SC_BAD_REQUEST)
  // 401
  object Unauthorized extends Error(HttpServletResponse.SC_UNAUTHORIZED)
  // 403
  object Forbidden extends Error(HttpServletResponse.SC_FORBIDDEN)
  // 404
  object NotFound extends Error(HttpServletResponse.SC_NOT_FOUND)

  object StaticFile {
    import java.io.File

    def apply(path: Seq[String]): Reply = {
      val file = new File(staticDirectory, path.mkString(File.separator))
      println("static file " + path.mkString(" / "))
      println(" file " + file)
      println(" exists " + file.exists)

      if (file.exists && file.isFile)
        SourceReply(scala.io.Source.fromFile(file)(scala.io.Codec.ISO8859)).commit
      else
        NotFound
    }

    def apply(f: String): Reply = apply(f.split('/'))
  }
}
