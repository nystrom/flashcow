package flashcow.core

import javax.servlet.http._
import javax.servlet.AsyncContext
import javax.servlet.ServletConfig
import scala.xml._

trait Routing extends Requests {
  self: WebApp =>

  import scala.collection.mutable.ListBuffer
  import scala.collection.mutable.Queue
  import java.util.concurrent.ConcurrentLinkedQueue
  import scala.collection.JavaConversions._

  import scala.actors.Actor
  import scala.actors.Actor._

  /**
   * A queue of saved connection contexts. Use a concurrent queue since
   * multiple threads will access it.
   * When a new event happens, all contexts in the queue are notified.
   */
  val contexts = new ConcurrentLinkedQueue[AsyncContext]

  /**
   * List of routes
   */
  val routes = new ListBuffer[Route]

  class Dispatcher(val c: AsyncContext) extends Actor {
    assert(false)
    def act = receive {
      case r: AsyncRequest =>
        assert(sender == responder)
        val c = r.ctx

        withContext(new Context(c.getRequest.asInstanceOf[HttpServletRequest],
                                c.getResponse.asInstanceOf[HttpServletResponse])) { xyz =>
          doDispatch(r)
        }
    }
  }

  def doDispatch(r: Request) = {
    val results = routes.reverse collectFirst {
      case route if route.accepts(r) => route.dispatch(r)
    }

    if (results isEmpty) {
      reply(NotFound)
    }
  }

  val useActors = false

  def reply(r: Reply) = {
    if (useActors)
      scala.actors.Actor.reply(r)
    else
      inContext {
        c =>
          val resp = c.response
          r.fill(resp)
      }
  }

  def dispatch(req: HttpServletRequest, resp: HttpServletResponse) = {
    if (useActors) {
      assert(false)
      val c = req.startAsync
      c.setTimeout(0)
      val r = new AsyncRequest(c)
      val dispatcher = new Dispatcher(c).start
      responder ! Forward(dispatcher, r)
    }
    else {
      val r = new Request(req)

      withContext(new Context(req, resp)) { xyz =>
        doDispatch(r)
      }
    }
  }

  case class Forward(a: Actor, r: Request)

  lazy val responder = actor {
    assert(false)
    while (true) {
      receive {
        case Forward(a: Dispatcher, r: Request) =>
          println("forwarding " + r)
          a ! r
        case r: Reply if sender.isInstanceOf[Dispatcher] =>
          sender match {
            case d: Dispatcher =>
              println("got reply " + r + " for " + new AsyncRequest(d.c))
              d.c.getResponse match {
                case resp: HttpServletResponse =>
                  println("sending response")
                  r.fill(resp)
                  println("complete")
                  d.c.complete
              }
          }
        case x => println("WTF " + x)
      }
    }
  }

  def route(handler: PartialFunction[Any, Unit]) = {
    val route = new Route(handler)
    routes += route
    route
  }

  class Route(handler: PartialFunction[Any, Unit]) {
    def accepts(req: Request) = handler.isDefinedAt(req)

    def dispatch(req: Request) = {
      println("route to " + req)

      txn {
        handler(req)
      }

      println("done with dispatch " + req)
    }
  }

  override def service(req: HttpServletRequest, resp: HttpServletResponse) {
    try {
      dispatch(req, resp)
    }
    catch {
      case ex: Exception =>
        resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, ex.getMessage)
    }
  }
}
