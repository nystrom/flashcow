package flashcow.core

import javax.servlet.http._
import scala.xml._
import scala.collection.JavaConversions._
import scala.util.matching.Regex
import javax.servlet.ServletConfig

// Generate unique identifiers.
object UniqueId {
  var n = 0
  def next: String = this.synchronized {
    n += 1
    "__" + n
  }
}

trait Vars {
  // Some utility classes that should live in the framework.

  // A trait for boxed mutable variables.
  // Supports the following operations:
  //   x()        // get the value
  //   x() = v    // set the value
  //   x.alter(f) // apply f to the value, storing the result
  //   x.map(f)   // apply f to the value, returning but not storing the result
  trait Var[A] extends Function0[A] {
    def update(x: A): Unit
    def alter(f: A => A) = update(f(apply))
    def map[B](body: A => B): B = body(apply)
  }

  // A utility class for thread-local variables.
  class ThreadLocal[T](init: => T) extends java.lang.ThreadLocal[T] with Var[T] {
    override def initialValue: T = init
    def apply = {
      val x = get
      // println("getting " + this + " -> " + x)
      x
    }
    def update(x: T) = {
      // println("setting " + this + " <- " + x)
      set(x)
    }
  }

  // An object representing a request context.
  // Just a wrapper around the request and response objects.
  class Context(val request: HttpServletRequest, val response: HttpServletResponse) {
    def session = request.getSession
    def uri = request.getRequestURI
  }

  // def withContext(c: Context)(body: => Unit): Unit = {
  //   try {
  //     currentContext() = c
  //     body
  //   }
  //   finally {
  //     currentContext() = null
  //   }
  // }

  import javax.servlet.AsyncContext

  def inContext[T](body: Context => T): T = {
    body(currentContext())
  }

  def withContext[T](c: Context)(body: Context => T): T = {
    val old = currentContext()
    try {
      currentContext() = c
      body(c)
    }
    finally {
      currentContext() = old
    }
  }

  // The current context. Each request has its own thread, so the context needs to be
  // thread local.
  var currentContext = new ThreadLocal[Context](null)

  abstract class KeyedVar[A](default: => A) extends Var[A] {
    val key = this.toString + UniqueId.next

    protected def get: Any

    def apply = get match {
      case null => default
      case x: A => x
      case x => throw new RuntimeException("value of wrong type: " + x)
    }
  }

  // A mutable variable whose lifetime is a session.
  class SessionVar[A](default: => A) extends KeyedVar[A](default) {
    def update(x: A) = currentContext().session.setAttribute(key, x)
    protected def get = currentContext().session.getAttribute(key)
    def init: Unit = ()
    def save: Unit = ()
  }

  // A mutable variable whose lifetime is a single request.
  class RequestVar[A](default: => A) extends KeyedVar[A](default) {
    def update(x: A) = currentContext().request.setAttribute(key, x)
    protected def get = {
      assert(currentContext() != null)
      assert(currentContext().request != null)
      currentContext().request.getAttribute(key)
    }
  }
}
