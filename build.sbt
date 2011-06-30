sbtPlugin := true

organization := "com.github.mpeltonen"

name := "sbt-idea"

version := "0.10.0-SNAPSHOT"

//sbtVersion := "0.10.1-20110629-052055"

scalacOptions := Seq("-deprecation", "-unchecked")

publishTo := Some(Resolver.file("GitHub Pages", file("../mpeltonen.github.com/maven/")))

resolvers += Resolver.url("Typesafe Snapshots" , url("http://repo.typesafe.com/typesafe/ivy-snapshots/"))