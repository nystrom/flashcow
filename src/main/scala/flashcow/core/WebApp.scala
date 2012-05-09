package flashcow.core

import javax.servlet.http._
import javax.servlet.AsyncContext
import javax.servlet.ServletConfig
import java.io.File
import scala.xml._

import org.squeryl.SessionFactory
import org.squeryl.Session
import org.squeryl.adapters.H2Adapter
import org.squeryl.PrimitiveTypeMode._

abstract class WebApp extends HttpServlet with Routing with Replies with View with Vars {
  def rootDirectory = System.getProperty("user.dir")
  def staticDirectory = rootDirectory + "/src/main/webapp/static".replace("/", File.separator)
  def templateDirectory = rootDirectory + "/src/main/webapp/templates".replace("/", File.separator)

  def database: Option[String] = None
  def databaseEnabled = database != None

  def txn(s: => Unit) = {
    try {
      if (databaseEnabled) {
        transaction { s }
      }
      else {
        s
      }
    }
    catch {
      case ex: java.sql.SQLException => // println(ex.getMessage)
      case ex: RuntimeException if ex.getCause.isInstanceOf[java.sql.SQLException] => // println(ex.getMessage)
    }
  }

  def initializeApp: Unit = {}

  override def init(config: ServletConfig) {
    database match {
      case Some(db) =>  
        Class.forName("org.h2.Driver")

        SessionFactory.concreteFactory = Some(() =>
          Session.create(
            java.sql.DriverManager.getConnection("jdbc:h2:" + rootDirectory + java.io.File.separator + db),
            new H2Adapter))
      case None =>
    }
    
    initializeApp
  }
}
