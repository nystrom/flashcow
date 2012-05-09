// see https://github.com/siasia/xsbt-web-plugin for more information on the
// jetty plugin

// import web settings
seq(webSettings :_*)

env in Compile := Some(file(".") / "jetty-env.xml" asFile)

name := "flash-servlet"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.eclipse.jetty" % "jetty-webapp" % "8.0.1.v20110908" % "container",
  "org.eclipse.jetty" % "jetty-plus" % "8.0.1.v20110908" % "container",
  "org.eclipse.jetty" % "jetty-http" % "8.0.1.v20110908" % "container",
  "org.eclipse.jetty" % "jetty-util" % "8.0.1.v20110908" % "container"
)

ivyXML := 
<dependency org="org.eclipse.jetty.orbit" name="javax.servlet" rev="3.0.0.v201112011016">
  <artifact name="javax.servlet" type="orbit" ext="jar"/>
</dependency>

resolvers += "twitter-repo" at "http://maven.twttr.com"

libraryDependencies ++= Seq(
  "com.twitter" % "util-core_2.9.1" % "1.12.8", "com.twitter" % "util-eval_2.9.1" % "1.12.8" 
)

libraryDependencies ++= Seq(
  "org.squeryl" %% "squeryl" % "0.9.5-RC1",
  "com.h2database" % "h2" % "1.2.127",
  "org.scala-lang" % "scalap" % "2.9.1" // needed for squeryl
)
  
// Drivers for other supported databases:
// val h2 = "com.h2database" % "h2" % "1.2.127"
// val mysqlDriver = "mysql" % "mysql-connector-java" % "5.1.10"
// val postgresDriver = "postgresql" % "postgresql" % "8.4-701.jdbc4"
// val msSqlDriver = "net.sourceforge.jtds" % "jtds" % "1.2.4"
// val derbyDriver = "org.apache.derby" % "derby" % "10.7.1.1"

// For testing.
libraryDependencies ++= Seq(
  "httpunit" % "httpunit" % "1.7",
  "rhino" % "js" % "1.7R2"
)


