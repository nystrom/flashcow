#!/bin/sh

java -Dfile.encoding=UTF8 -Dsbt.log.noformat=true -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256m -Dsbt.boot.directory=$HOME/.sbt/boot/ -jar sbt-launch.jar "$@"
