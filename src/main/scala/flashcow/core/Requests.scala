package flashcow.core

import javax.servlet.http._
import javax.servlet.AsyncContext
import javax.servlet.ServletConfig
import scala.xml._

trait Requests {
  self: WebApp =>

  import scala.collection.mutable.ListBuffer
  import scala.collection.mutable.Queue
  import java.util.concurrent.ConcurrentLinkedQueue
  import scala.collection.JavaConversions._

  object -> {
    def unapply(p: Any): Option[(Any,Any)] = {
      p match {
        case (a,b) => Some((a,b))
      } 
    }
  }
  object and {
    def unapply(r: Any) = Some((r, r))
  }

  trait FromString[A] {
    def fromString(s: String): Option[A]
  }


  implicit object IntFromString extends FromString[Int] {
    def fromString(s: String) = try {
      Some(s.toInt)
    } catch {
      case ex: NumberFormatException => None
    }
  }

  implicit object LongFromString extends FromString[Long] {
    def fromString(s: String) = try {
      Some(s.toLong)
    } catch {
      case ex: NumberFormatException => None
    }
  }

  implicit object StringFromString extends FromString[String] {
    def fromString(s: String) = Some(s)
  }

  def testRequestMatching(r: Request) = {
    val q = param[Int]("q")
    r match {
      case Path("/") and q(x) => true
      case Get(Path("/") and Path("/")) => true
      case Get(Path("/")) => true
      case _ => false
    }
  }

  def param[A: FromString](key: String) = new {
    def unapply(r: Request): Option[A] = {
      r.param(key) flatMap { v => implicitly[FromString[A]].fromString(v) }
    }
  }

  object StartsWithSlash {
    def unapply(r: Request) = r.uri.startsWith("/")
  }

  object EndsWithSlash {
    def unapply(r: Request) = r.uri.endsWith("/")
  }

  object Path {
    def unapplySeq(r: Request): Option[List[String]] = {
      println("destruct " + r + " -> " + r.path)
      Some(r.path)
    }
  }

  object Head {
    def unapply(req: Request) =
      if (req.method == "HEAD") Some(req)
      else None
  }

  object Post {
    def unapply(req: Request) =
      if (req.method == "POST") Some(req)
      else None
  }

  object Put {
    def unapply(req: Request) =
      if (req.method == "PUT") Some(req)
      else None
  }

  object Delete {
    def unapply(req: Request) =
      if (req.method == "DELETE") Some(req)
      else None
  }

  object Get {
    def unapply(req: Request) =
      if (req.method == "GET") Some(req)
      else None
  }

  class AsyncRequest(val ctx: AsyncContext) extends Request(ctx.getRequest.asInstanceOf[HttpServletRequest])
  
  class Request(val raw: HttpServletRequest) {
    def uri = raw.getRequestURI

    def path: List[String] = {
      val s = raw.getRequestURI
      val p = s.split('/').filter(_!="")
      p.toList
    }
    
    def param(key: String): Option[String] = {
      val v = raw.getParameter(key)
      if (v != null)
        Some(v)
      else
      None
    }
    
    def params: Map[String, List[String]] = raw.getParameterMap.toMap.mapValues(_.toList)
    
    def headers: Map[String, List[String]] = {
      val hs = 
        for (key <- raw.getHeaderNames; value <- raw.getHeaders(key))
          yield (key, value)
      hs.toList.groupBy(_._1).mapValues { case vs => vs.map(_._2) }
    }
    def method = raw.getMethod.toUpperCase

    override lazy val toString = method + " " + uri + (if (raw.getQueryString != null && method == "GET") "?" + raw.getQueryString else "")
  }
}
