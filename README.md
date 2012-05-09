Flashcow
========

Flashcow is simple Scala web framework originally built for teaching purposes.

This is still very much a work in progress.  The code is insecure and should
NOT be deployed in a real-world environment.

The framework includes an eponymous flashcard application.
Currently the application is not separate from the framework. This is on the TODO list.


Credits
-------

The code was written by Nate Nystrom (nate.nystrom@usi.ch).

Some code is derived from Michael Schmitz's HelloWorld Scala Servlet at:

    https://github.com/schmmd/helloworld-scala-servlet

I made some changes to make the servlet easier to use out-of-the-box.  The
steps I took are described below.

Building and running
--------------------

You can import this directory as an eclipse project. The easiest way to run
the servlet, however, is from the terminal.  You will need sbt (simple build
tool). I have included sbt-launch.jar in this directory.

I am using a script to invoke sbt. You can run this script as

   ./sbt <options>

I would copy the script to ~/bin/sbt where ~/bin is in my path, but it's not
necessary.

To invoke sbt without a script, do:

    java -jar sbt-launch.jar <options>

To run sbt from your terminal, type:

    ./sbt

This will give you a prompt that looks like this:

    >

At this prompt, type:

    > container:start

This will compile the application and start the server on port 8080.
You should then see "Hello world!" at http://localhost:8080/.

To stop the server:

    > container:stop

To reload the server after recompiling:

    > container:reload

Sometimes it's nice to have your project continuously deployed (deployed after
each saved change):

    > container:start
    > ~aux-compile

================================

Here are the steps I took to build this servlet from scratch.
All commands, unless noted, are executed in the terminal.

1. First get the HelloWorld servlet.

    git clone https://github.com/schmmd/helloworld-scala-servlet
    cd helloworld-scala-servlet

2. Get sbt 0.11

    curl -O http://typesafe.artifactoryonline.com/typesafe/ivy-releases/org.scala-tools.sbt/sbt-launch/0.11.2/sbt-launch.jar

Create the following script as ./sbt:

    --------------------------------
    #!/bin/sh
    java -Dfile.encoding=UTF8 -Dsbt.log.noformat=true -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -Dsbt.boot.directory=$HOME/.sbt/boot/ -jar sbt-launch.jar "$@"
    --------------------------------

3. Get the sbteclipse plugin.

Append the following (without the ----'s) to the file project/plugins.sbt:

    --------------------------------

    addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.0.0")
    --------------------------------

Note the blank line is important.

4. Fix dependencies.

   Change 0.2.9 to 0.2.10 in project/plugins.sbt

5. Rename the project.

   Change "helloworld" in build.sbt to "simple-servlet" (or whatever you want).
   Change the class name in src/main/webapp/WEB-INF/web.xml.

6. Download the dependencies.

    sbt update

7. Build an eclipse project.

    sbt eclipse

This will create an appropriately configured .project.

Now, you can edit the code in Eclipse.

