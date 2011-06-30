package org.sbtidea

import sbt._

object SbtIdeaModuleMapping {

  def toIdeaLib(instance: ScalaInstance) = {
    IdeaLibrary("scala-" + instance.version, List(instance.libraryJar, instance.compilerJar),
      instance.extraJars.filter(_.getAbsolutePath.endsWith("docs.jar")),
      instance.extraJars.filter(_.getAbsolutePath.endsWith("-sources.jar")))
  }

  private def equivModule(m1: ModuleID, m2: ModuleID, scalaVersion: String) = {
    def name(m: ModuleID): String =
      if (m.crossVersion) m.name + "_" + scalaVersion else m.name

    m1.organization == m2.organization && name(m1) == name(m2)
  }

  private def ideaLibFromModule(moduleReport: ModuleReport, useMavenRepo: Boolean) = {
    val module = moduleReport.module
    val name = useMavenRepo match {
      case true =>"Maven: %s:%s:%s".format(module.organization, module.name, module.revision)
      case false => module.organization + "_" + module.name + "_" + module.revision
    }
    IdeaLibrary(name,
      classes = moduleReport.artifacts.collect{ case (artifact, file) if (artifact.classifier == None || artifact.classifier == Some(("classes"))) =>
        depFile(module, artifact, file, useMavenRepo)},
      javaDocs = moduleReport.artifacts.collect{ case (artifact, file) if (artifact.classifier == Some("javadoc")) =>
        depFile(module, artifact, file, useMavenRepo)},
      sources = moduleReport.artifacts.collect{ case (artifact, file) if (artifact.classifier == Some("sources")) =>
        depFile(module, artifact, file, useMavenRepo)}
    )
  }

  private def depFile(id: ModuleID, a: Artifact, ivyFile: File, useMavenRepo: Boolean): File = {
    useMavenRepo match {
      case true => {
        val f = mavenFile(id, a)
        if (f.exists) f else ivyFile
      }
      case false => ivyFile
    }
  }

  private def mavenFile(id: ModuleID, a: Artifact, overrideClassifier: Option[String] = None): File = {
    val classifier = (overrideClassifier, a.classifier) match {
      case (Some(c), _) => "-"+c
      case (None, Some(c)) => "-"+c
      case (None, None) => ""
    }
//    println("Creating maven file: %s, %s, %s".format(id, a, overrideClassifier))
    val f = localM2Repo / id.organization.replace(".", "/") / id.name / id.revision /
          "%s-%s%s.%s".format(id.name, id.revision, classifier, a.extension)
    // If the base file doesn't exist, use the "classes" classifier
    (f.exists, overrideClassifier) match {
      case (false, None) => mavenFile(id, a, Some("classes"))
      case _ => f
    }
  }

  private def toScope(conf: String) = {
    import org.sbtidea.IdeaLibrary._
    conf match {
      case "compile" => CompileScope
      case "runtime" => RuntimeScope
      case "test" => TestScope
      case "provided" => ProvidedScope
      case _ => CompileScope
    }
  }

  private def mapToIdeaModuleLibs(configuration: String, modules: Seq[ModuleReport], deps: Keys.Classpath, useMavenLibs: Boolean) = {
    val scope = toScope(configuration)
    val depFilter = libDepFilter(deps) _

//    println("%s, %s".format(configuration, deps))

    modules.map( moduleReport => (IdeaModuleLibRef(scope, ideaLibFromModule(moduleReport, useMavenLibs)), moduleReport.module))
  }

  private def libDepFilter(deps: Keys.Classpath)(moduleReport: ModuleReport): Boolean = {
    val exists = deps.files.exists(dep => moduleReport.artifacts.exists(_._2 == dep))
//    println("Exists: %s - %s".format(exists, moduleReport))
    exists
  }

  def convertDeps(report: UpdateReport, deps: Keys.Classpath, scalaVersion: String, useMavenLibs: Boolean): Seq[(IdeaModuleLibRef, ModuleID)] = {
    Seq("compile", "runtime", "test", "provided").flatMap(report.configuration).foldLeft(Seq[(IdeaModuleLibRef, ModuleID)]()) { (acc, configReport) =>
      val filteredModules = configReport.modules.filterNot(m1 =>
        acc.exists { case (_, m2) => equivModule(m1.module, m2, scalaVersion) })
      acc ++ mapToIdeaModuleLibs(configReport.configuration, filteredModules, deps, useMavenLibs)
    }
  }

  def addClassifiers(ideaModuleLibRefs: Seq[(IdeaModuleLibRef, ModuleID)], report: UpdateReport, useMavenLibs: Boolean): Seq[(IdeaModuleLibRef, ModuleID)] = {

    /* Both retrieved from UpdateTask, so we don't need to deal with crossVersion here */
    def equivModule(m1: ModuleID, m2: ModuleID): Boolean =
      m1.name == m2.name && m1.organization == m2.organization && m1.revision == m2.revision

    val modifiedModuleLibRefs = {

      report.configurations.flatMap { configReport =>
          
        configReport.modules.flatMap { moduleReport =>

          ideaModuleLibRefs.find { case (moduleLibRef, moduleId) =>
            moduleLibRef.config == toScope(configReport.configuration) && equivModule(moduleReport.module, moduleId)
          } map { case (moduleLibRef, moduleId) =>

            val ideaLibrary = {
              val il = ideaLibFromModule(moduleReport, useMavenLibs)
              il.copy(classes = il.classes ++ moduleLibRef.library.classes,
                      javaDocs = il.javaDocs ++ moduleLibRef.library.javaDocs,
                      sources = il.sources ++ moduleLibRef.library.sources)
            }

            moduleLibRef.copy(library = ideaLibrary) -> moduleId
          }
        }
      }
    }

    val unmodifiedModuleLibRefs = ideaModuleLibRefs.filterNot { case (_, m1) =>
      modifiedModuleLibRefs.exists { case (_, m2) => equivModule(m1, m2) }
    }

    modifiedModuleLibRefs ++ unmodifiedModuleLibRefs

  }

  lazy val localM2Repo = Path.userHome / ".m2/repository"
}