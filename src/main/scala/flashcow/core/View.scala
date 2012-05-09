package flashcow.core

import javax.servlet.http._
import javax.servlet.AsyncContext
import scala.xml._

trait View {
  self: WebApp with Vars with Replies =>

  /** Write XML back in a response. */
  def writeXML(resp: HttpServletResponse, ns: NodeSeq) = resp.getWriter.write(new PrettyPrinter(72, 2).formatNodes(ns))

  def writeHTMLDoc(resp: HttpServletResponse, ns: => NodeSeq) = {
    resp.setContentType("text/html")
    resp.getWriter.write("<!DOCTYPE html>")
    writeXML(resp, ns)
  }

  def emptyReply(resp: HttpServletResponse) = {
    resp.setStatus(200)
    resp.setContentLength(0)
    resp.setContentType("application/xml")
  }

  def template(f: String, bind: (String, Any)*): NodeSeq = {
    template(f, bind.toMap)
  }

  def template(f: String, bind: Map[String, Any]): NodeSeq = {
    val template = templateXML(f)
    processTemplate(template, bind)
  }


  import java.io.File
  import scala.xml._

  import scala.collection.mutable.HashMap

  def memo[A, B](m: => HashMap[A,B])(x: A)(y: => B) = m.getOrElseUpdate(x, y)
  def nomemo[A, B](x: A)(y: => B) = y

  object templateCache extends RequestVar[HashMap[String, NodeSeq]](new HashMap[String, NodeSeq])
  val shouldMemo = true
  def templateMemo: String => (=> NodeSeq) => NodeSeq = {
    val m = templateCache()
    if (m == null) nomemo _
    else memo(m) _
  }

  def templateXML(f: String): NodeSeq = {
    val g = if (f.startsWith("/")) f else "/" + f

    templateMemo(g) {
      println("missed cache for " + g)
      val fn = g.replace("/", File.separator) + ".xml"
      val root = new File(templateDirectory)
      val file = new File(root, fn)

      if (! file.getCanonicalPath.startsWith(root.getCanonicalPath))
        throw new java.io.IOException("Bad boy!")

      if (file.exists && file.isFile) {
        fixQuotes(XML.loadFile(file))
      }
      else {
        throw new java.io.FileNotFoundException(fn)
      }
    }
  }

  def fixQuotes(template: NodeSeq): NodeSeq = {
    import scala.xml.transform._

    object FixQuotes extends BasicTransformer {
      override protected def unchanged(n: Node, ns: Seq[Node]) = ns.length == 1 && (ns.head eq n)
      override def transform(n: Node): Seq[Node] = n match {
        case n: Text => new Unparsed(n.text)
        case n => super.transform(n)
      }
    }

    FixQuotes.transform(template)
  }

  val debugTemplates = false

  import scala.xml.transform._

  object templateProcessCount extends RequestVar[Int](0)

  def processTemplate(template: NodeSeq, bind: Map[String, Any]): NodeSeq = {
    val count = templateProcessCount()
    templateProcessCount.alter(_+1)

    if (debugTemplates) {
      println(count + " >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
      println(template)
      println("bind = " + bind)
      println(count + " <-----------------------------------------------------------")
    }

    val f = templateTransformer(bind)
    val t = f.transform(template)

    if (debugTemplates) {
      println(count + " >-----------------------------------------------------------")
      println(t)
      println(count + " <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<")
    }

    // if (count == 0) {
    //   assert((t \\ "surround").isEmpty)
    //   assert((t \\ "embed").isEmpty)
    //   assert((t \\ "foreach").isEmpty)
    //   assert((t \\ "bind").isEmpty)
    // }

    t
  }

  // def myTransformer(bind: Map[String, Any]): Seq[Node] => Seq[Node] = {
  //   // Based on Kiama / Stratego rewriters
  //   trait Strategy extends NodeSeq => Option[NodeSeq] {
  //     p =>   // alias this as p

  //     def apply(r: NodeSeq): Option[NodeSeq]

  //     // Sequential composition. Apply p, then q.
  //     def <*(q: => Strategy): Strategy = new Strategy {
  //       def apply(t1: NodeSeq): Option[NodeSeq] = p(t1) flatMap q
  //     }

  //     // Deterministic choice.
  //     def <+(q: => Strategy): Strategy = new Strategy {
  //       def apply(t1: NodeSeq) = p(t1) match {
  //         case Some(t2) => q(t2)
  //         case None => None
  //       }
  //     }

  //     // ND choice
  //     def +(q: Strategy): PlusStrategy = new PlusStrategy(p, q)

  //     // Cond choice: c < l + r
  //     def <(lr => PlusStrategy): Strategy = new Strategy {
  //       def apply(t1: NodeSeq) = p(t1) match {
  //         case Some(t2) => lr.p(t2)
  //         case None => lr.q(t1)
  //       }
  //     }
  //   }

  //   class PlusStrategy(val p: => Strategy, val q: => Strategy) extends Strategy {
  //     def apply(t: NodeSeq) = (p <+ q)(t)
  //   }

  //   implicit def sf(f: NodeSeq => Option[NodeSeq]): Strategy = new Strategy {
  //     def apply(t: NodeSeq) = f(t)
  //   }

  //   implicit def spf(f: PartialFunction[NodeSeq => Option[NodeSeq]]): Strategy = new Strategy {
  //     def apply(t: NodeSeq) = if (f.isDefinedAt(t)) f(t) else None
  //   }


  //   implicit def rf(f: NodeSeq => NodeSeq): Strategy = new Strategy {
  //     def apply(t: NodeSeq) = Some(f(t))
  //   }

  //   implicit def rpf(f: PartialFunction[NodeSeq => NodeSeq]): Strategy = new Strategy {
  //     def apply(t: NodeSeq) = if (f.isDefinedAt(t)) Some(f(t)) else None
  //   }

  //   def rfs(f: PartialFunction[NodeSeq, Strategy]): Strategy = new Strategy {
  //     def apply(t: Term) = if (f.isDefinedAt(t)) f(t)(t) else None
  //   }

  //   def build(t: => NodeSeq): Strategy = rf(_ => t)
  //   def option(o: => Option[NodeSeq]): Strategy = sfo(_ => o)

  //   val fail = option(None)
  //   val id = sf(t => Some(t))

  //   // traverse to all children.
  //   def all(s: => Strategy): Strategy = {
  //     def apply(ns: NodeSeq) = ns match {
  //       case e: Elem => Some(e.copy(child = e.child map s))
  //       case n => Some(n)
  //     }
  //   }

  //   // traverse to all descendents
  //   def bottomup(s: => Strategy): Strategy = {
  //     def apply(ns: NodeSeq) = ns map { n => s(all(s)(n)) }
  //   }

  //   def one(s: => Strategy): Strategy = {
  //     def apply(ns: NodeSeq) = ns map { n => s(n) }
  //   }


  //   def recurse(p: Node => Boolean, f: Node => Node)(n: Node): Node =
  //     if (p(n))
  //       f(n)
  //     else
  //       n match {
  //         case e: Elem =>
  //           e.copy(e.prefix, e.label, e.attributes, e.scope, e.child.map(recurse(pred, trans) _))
  //         case n => n
  //       }

  //   def transform(n: Node, pf: PartialFunction[Node, Node]) =
  //     transformIf(n, pf.isDefinedAt(_), pf.apply(_));

  //   class Transform(f: Node => Node) extends Seq[Node] => Seq[Node] {
  //     def apply(ns: Seq[Node]) = ns map f
  //   }
  // }

  // Template

  object LazyTemplate {
    // Should be a session var?
    val cache = new scala.collection.mutable.HashMap[String, (NodeSeq, Map[String, Any])]

    def apply(id: String): Reply =
      LazyTemplate.synchronized {
        cache.get(id) match {
          case Some((ns: NodeSeq, bind: Map[String, Any])) =>
            // We've used the template once, delete it.
            cache.remove(id)
            val xml = processTemplate(ns, bind)
            Ok + HTML(xml) + Commit
          case _ =>
            NotFound
        }
      }
  }

  def lazyTemplate(ns: NodeSeq, bind: Map[String, Any]): NodeSeq = {
    val id = UniqueId.next

    LazyTemplate.synchronized {
      LazyTemplate.cache += (id -> (ns, bind))
    }

    val js = """
      $(function() {
        $.get("/flashcow/lazy", { id: "%s" }, function (x) {
          $("div#%s").html(x);
        });
      });
    """.format(id, id)

    <div>
      <div id={id}>{ns}</div>
      <script>
        {new Unparsed(js)}
      </script>
    </div>
  }

  def templateTransformer(bind: Map[String, Any]): BasicTransformer = {
    // Top-down transformation.
    class TopDown(bind: Map[String, Any]) extends BasicTransformer {
      override def transform(n: Node): Seq[Node] = {
        // println("transforming " + n)
      n match {
        case elem: Elem if elem.label == "surround" =>
          val outerName = (elem \ "@with").text
          val innerName = (elem \ "@at").text
          val inner = elem.child
          val outer = templateXML(outerName)
          val subst = new TopDown(bind + (innerName -> inner))
          subst.transform(outer)
        case elem: Elem if elem.label == "embed" =>
          val innerName = (elem \ "@name").text
          val inner = templateXML(innerName) // load and process the template
          super.transform(inner)
        case elem: Elem if elem.label == "lazy" =>
          lazyTemplate(elem.child, bind)
        case elem: Elem if elem.label == "foreach" =>
          val name = (elem \ "@name").text
          val list = (elem \ "@in").text
          bind.get(list) match {
            case Some(ns: Seq[Any]) =>
              NodeSeq.fromSeq {
                val nns =
                  for (n <- ns)
                    yield {
                      val b = bind collect {
                        case (k, pf: PartialFunction[Any, Any]) if k.startsWith(name + ".") && pf.isDefinedAt(n) => (k, pf(n))
                      }
                      val subst = new TopDown(bind ++ b)
                      subst.transform(elem.child)
                    }
                nns.flatten
              }
            case Some(v) =>
              new Unparsed("<!-- binding for sequence " + list + " is not a sequence (" + v + ") -->")
            case None => elem
          }
        case elem: Elem if elem.label == "bind" =>
          val name = (elem \ "@name").text
          bind.get(name) match {
            case Some(ns: NodeSeq) =>
              super.transform(ns)
            case Some(seq: Seq[Node]) =>
              val ns = NodeSeq.fromSeq(seq)
              super.transform(ns)
            case Some(pf: PartialFunction[_,_]) => new Unparsed("<!-- " + name + ": [" + pf + "] -->")
            case Some(s: String) => new Unparsed(s)
            case Some(v) => new Unparsed(v.toString)
            case None => elem
          }
        case n =>
          super.transform(n)
      }
      }
    }

    if (true) return new TopDown(bind)

    val newBind = bind.filter {
      // case (k, s: Seq[_]) => true // oh!
      case (k, pf: PartialFunction[_,_]) if (k contains ".") => false
      case (k, v) => true
    }

    // println("bind = " + bind)
    // println("newBind = " + newBind)

    class Subst(bind: Map[String, Any], recurse: Boolean) extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case elem: Elem if elem.label == "bind" =>
          val name = (elem \ "@name").text
          bind.get(name) match {
            case Some(ns: NodeSeq) =>
              if (debugTemplates)
                println("bind " + name + " -> " + ns.mkString.replace('\n', ' ').take(20))
              // println("subst " + bind)
              // println(" newBind " + newBind)
              // println(" in " + ns)
              if (recurse && ! (ns \\ "bind").isEmpty) {
                val subst = new RuleTransformer(new Subst(bind, true))
                subst.transform(ns)
              }
              else
                ns
            case Some(seq: Seq[Node]) =>
              val ns = NodeSeq.fromSeq(seq)
              if (debugTemplates)
                println("bind " + name + " -> " + ns.mkString.replace('\n', ' ').take(20))
              // println("subst " + bind)
              // println(" newBind " + newBind)
              // println(" in " + ns)
              if (recurse && ! (ns \\ "bind").isEmpty) {
                val subst = new RuleTransformer(new Subst(bind, true))
                subst.transform(ns)
              }
              else
                ns
            // case Some(xs: Seq[_]) => NodeSeq.fromSeq(xs.asInstanceOf[Seq[Any]].map(....))
            case Some(pf: PartialFunction[_,_]) => new Unparsed("<!-- " + name + ": [" + pf + "] -->")
            case Some(s: String) => new Unparsed(s)
            case Some(v) => new Unparsed(v.toString)
            case None => elem
          }
        case n => n
      }
    }

    object ProcessSurround extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case elem: Elem if elem.label == "surround" =>
          val outerName = (elem \ "@with").text
          val innerName = (elem \ "@at").text
          if (debugTemplates)
            println("> surround " + innerName + " with " + outerName)
          val inner = elem.child
          // val outer = template(outerName, innerName -> inner)
          // outer
          val outer = templateXML(outerName)
          val subst = new RuleTransformer(new Subst(Map(innerName -> inner), false))
          val newOuter = subst.transform(outer)
          if (debugTemplates)
            println("< surround " + innerName + " with " + outerName + " -> " + newOuter.mkString.replace('\n', ' ').take(20))
          newOuter
        case n => n
      }
    }

    object ProcessEmbed extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case elem: Elem if elem.label == "embed" =>
          val innerName = (elem \ "@name").text
          if (debugTemplates)
            println("embed " + innerName)
          val inner = templateXML(innerName) // load and process the template
          inner
        case n => n
      }
    }

    class ProcessForeach(bind: Map[String, Any]) extends RewriteRule {
      override def transform(n: Node): Seq[Node] = n match {
        case elem: Elem if elem.label == "foreach" =>
          val name = (elem \ "@name").text
          val list = (elem \ "@in").text
          if (debugTemplates)
            println("foreach " + name + " in " + list)
          // println("elem = " + elem)
          // println("name = " + name)
          // println("list = " + list)

          bind.get(list) match {
            case Some(ns: Seq[Any]) =>
              NodeSeq.fromSeq {
                val nns =
                  for (n <- ns)
                    yield {
                      val b = bind collect {
                        case (k, pf: PartialFunction[Any, Any]) if k.startsWith(name + ".") && pf.isDefinedAt(n) => (k, pf(n))                       case (k, pf: PartialFunction[Any, Any]) if k.startsWith(name + ".") && pf.isDefinedAt(n) => (k, pf(n))
                      }
                      val subst = new RuleTransformer(new Subst(b, true))
                      val newChild = subst.transform(elem.child)
                      newChild
                    }
                nns.flatten
              }
            case Some(v) =>
              new Unparsed("<!-- binding for sequence " + list + " is not a sequence (" + v + ") -->")
            case None => elem
          }
        case n => n
      }
    }

    class FixPoint(f: BasicTransformer) extends BasicTransformer {
      override final def transform(n: Node): Seq[Node] = {
        val t = f.transform(n)
        if (unchanged(n, t))
          t
        else
          transform(t)
      }
    }

    class Then(f: BasicTransformer, g: BasicTransformer) extends BasicTransformer {
      override final def transform(n: Node): Seq[Node] = {
        g.transform(f.transform(n))
      }
    }

    val b = new FixPoint(new RuleTransformer(ProcessSurround, ProcessEmbed, new ProcessForeach(bind), new Subst(newBind, true)))
    if (true) return b

    val noBinding = new FixPoint(new RuleTransformer(ProcessSurround, ProcessEmbed))
    val foreach = new RuleTransformer(new ProcessForeach(bind))
    val subst = new RuleTransformer(new Subst(newBind, true))

    new Then(new Then(noBinding, foreach), subst)
  }
}
