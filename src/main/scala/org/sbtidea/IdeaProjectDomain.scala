package org.sbtidea

import java.io.File
// cheating for now
import sbt.ScalaInstance

class IdeaProjectDomain

object IdeaLibrary {
  sealed abstract class Scope(val configName: String)
  case object CompileScope extends Scope("")
  case object RuntimeScope extends Scope("RUNTIME")
  case object TestScope extends Scope("TEST")
  case object ProvidedScope extends Scope("PROVIDED")
}

case class IdeaLibrary(name: String, classes: Seq[File], javaDocs: Seq[File], sources: Seq[File])

case class IdeaModuleLibRef(config: IdeaLibrary.Scope, library: IdeaLibrary)

case class Directories(sources: Seq[File], resources: Seq[File], outDir: File)

case class SubProjectInfo(baseDir: File, name: String, dependencyProjects: List[String], compileDirs: Directories,
                          testDirs: Directories, libraries: Seq[IdeaModuleLibRef], scalaInstance: ScalaInstance,
                          ideaGroup: Option[String], webAppPath: Option[File], basePackage: Option[String])

case class IdeaProjectInfo(baseDir: File, name: String, childProjects: List[SubProjectInfo], ideaLibs: List[IdeaLibrary])

case class IdeaUserEnvironment(webFacet: Boolean)

case class IdeaProjectEnvironment(projectJdkName :String, javaLanguageLevel: String,
                                  includeSbtProjectDefinitionModule: Boolean, projectOutputPath: Option[String],
                                  excludedFolders: String, compileWithIdea: Boolean, modulePath: Option[String])