package org.sbtidea

import sbt._

object SbtIdeaModuleMapping {

  def toIdeaLib(instance: ScalaInstance) = {
    IdeaLibrary("scala-" + instance.version, List(instance.libraryJar, instance.compilerJar),
      instance.extraJars.filter(_.getAbsolutePath.endsWith("docs.jar")),
      instance.extraJars.filter(_.getAbsolutePath.endsWith("-sources.jar")))
  }

  /**
   * Extracts IDEA libraries from the keys:
   *
   *   * `externalDependencyClasspath`
   *   * `update`
   *   * `updateClassifiers`
   *   * `updateSbtClassifiers`
   *   * `unmanagedClasspath`
   */
  final class LibrariesExtractor(buildStruct: Load.BuildStructure, state: State, projectRef: ProjectRef, logger: Logger,
                                 scalaInstance: ScalaInstance, withClassifiers: Boolean, useLocalMaven: Option[Boolean]) {

    def allLibraries: Seq[IdeaModuleLibRef] = managedLibraries ++ unmanagedLibraries

    val useMaven = useLocalMaven.getOrElse(false)

    /**
     * Creates an IDEA library entry for each entry in `externalDependencyClasspath` in `Test` and `Compile.
     *
     * The result of `update`, `updateClassifiers`, and is used to find the location of the library,
     * by default in $HOME/.ivy2/cache
     */
    def managedLibraries: Seq[IdeaModuleLibRef] = {
      val deps = evaluateTask(Keys.externalDependencyClasspath in Configurations.Test) match {
        case Some(Value(deps)) => deps
        case _ => logger.error("Failed to obtain dependency classpath"); throw new IllegalArgumentException()
      }
      val libraries: Seq[(IdeaModuleLibRef, ModuleID)] = evaluateTask(Keys.update) match {

        case Some(Value(report)) =>
          val libraries: Seq[(IdeaModuleLibRef, ModuleID)] = convertDeps(report, deps, scalaInstance.version, useMaven)

          if (withClassifiers) {
            evaluateTask(Keys.updateClassifiers) match {
              case Some(Value(report)) => addClassifiers(libraries, report, useMaven)
              case _ => libraries
            }
          } else libraries

        case _ => Seq.empty
      }

      libraries.map(_._1)
    }

    /**
     * Creates an IDEA library entry for each entry in `unmanagedClasspath` in `Test` and `Compile.
     *
     * If the entry is both in the compile and test scopes, it is only added to the compile scope.
     *
     * source and javadoc JARs are detected according to the Maven naming convention. They are *not*
     * added to the classpath, but rather associated with the corresponding binary JAR.
     **/
    def unmanagedLibraries: Seq[IdeaModuleLibRef] = {
      def unmanagedLibrariesFor(config: Configuration): Seq[IdeaModuleLibRef] = {
        evaluateTask(Keys.unmanagedClasspath in config) match {
          case Some(Value(unmanagedClassPathSeq)) =>

            /**Uses naming convention to look for an artifact with `classifier` in the same directory as `orig`. */
            def classifier(orig: File, classifier: String): Option[File] = file(orig.getAbsolutePath.replace(".jar", "-%s.jar".format(classifier))) match {
              case x if x.exists => Some(x)
              case _ => None
            }
            for {
              attributedFile <- unmanagedClassPathSeq
              f = attributedFile.data
              if Seq("sources", "javadoc").forall(classifier => !f.name.endsWith("-%s.jar".format(classifier)))
              scope = toScope(config.name)
              sources = classifier(f, "sources").toSeq
              javadocs = classifier(f, "javadoc").toSeq
              ideaLib = IdeaLibrary(f.getName, classes = Seq(f), sources = sources, javaDocs = javadocs)
            } yield IdeaModuleLibRef(scope, ideaLib)
          case _ => Seq()
        }
      }

      val compileUnmanagedLibraries = unmanagedLibrariesFor(Configurations.Compile)
      val testUnmanagedLibraries = unmanagedLibrariesFor(Configurations.Test).filterNot(libRef => compileUnmanagedLibraries.exists(_.library == libRef.library))
      compileUnmanagedLibraries ++ testUnmanagedLibraries
    }

    private def evaluateTask[T](taskKey: sbt.Project.ScopedKey[sbt.Task[T]]) =
      EvaluateTask.evaluateTask(buildStruct, taskKey, state, projectRef, false, EvaluateTask.SystemProcessors)
  }

  private def equivModule(m1: ModuleID, m2: ModuleID, scalaVersion: String) = {
    def name(m: ModuleID): String = if (m.crossVersion) m.name + "_" + scalaVersion else m.name

    m1.organization == m2.organization && name(m1) == name(m2)
  }

  private def ideaLibFromModule(moduleReport: ModuleReport, useMavenRepo: Boolean, libraryDep: Option[ModuleID] = None): IdeaLibrary = {
    val module = moduleReport.module

    val name = useMavenRepo match {
      case true =>"Maven: %s:%s:%s".format(module.organization, module.name, module.revision)
      case false => module.organization + "_" + module.name + "_" + module.revision
    }

    def findByClassifier(classifier: Option[String]) = moduleReport.artifacts.collect {
      case (artifact, file) if (artifact.classifier == classifier) => depFile(module, artifact, file, useMavenRepo)
    }

    IdeaLibrary(name,
      classes = findByClassifier(None) ++ findByClassifier(Some("classes")),
      javaDocs = findByClassifier(Some("javadoc")),
      sources = findByClassifier(Some("sources")),
      projectDir = libraryDep.flatMap( d => d.extraAttributes.get(SbtIdeaPlugin.LocalProjectRootAttributeName).map(f => new File(f))))

//    IdeaLibrary(name,
//      classes = moduleReport.artifacts.collect {
//        case (artifact, file) if (artifact.classifier == None || artifact.classifier == Some("classes")) =>
//          depFile(module, artifact, file, useMavenRepo)
//      },
//      javaDocs = moduleReport.artifacts.collect{
//        case (artifact, file) if (artifact.classifier == Some("javadoc")) =>
//          depFile(module, artifact, file, useMavenRepo)
//        case (artifact, file) if (artifact.classifier == None || artifact.classifier == Some("classes")) =>
//          depFile(module, artifact, file, useMavenRepo, Some("javadoc"))
//      },
//      sources = moduleReport.artifacts.collect{
//        case (artifact, file) if (artifact.classifier == Some("sources")) =>
//          depFile(module, artifact, file, useMavenRepo)
//        case (artifact, file) if (artifact.classifier == None || artifact.classifier == Some("classes")) =>
//          depFile(module, artifact, file, useMavenRepo, Some("sources"))
//      }
//    )
  }

  private def depFile(id: ModuleID, a: Artifact, ivyFile: File, useMavenRepo: Boolean, overrideClassifier: Option[String] = None): File = {
    useMavenRepo match {
      case true => {
        val f = mavenFile(id, a, overrideClassifier)
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

  private def mapToIdeaModuleLibs(configuration: String, modules: Seq[ModuleReport], deps: Keys.Classpath,
                                  scalaVersion: String, useMavenLibs: Boolean) = {

    val scope = toScope(configuration)
    val depFilter = libDepFilter(deps.flatMap(_.get(Keys.moduleID.key)), scalaVersion) _

//    println("%s, %s".format(configuration, deps))

    //modules.map( moduleReport => (IdeaModuleLibRef(scope, ideaLibFromModule(moduleReport, useMavenLibs)), moduleReport.module))

    modules.filter(modReport => depFilter(modReport.module)).map(moduleReport => {
      (IdeaModuleLibRef(scope, ideaLibFromModule(moduleReport, useMavenLibs,
        libraryDep=libraryDepForModule(deps, scalaVersion, moduleReport.module))), moduleReport.module)
    })
  }

  def libraryDepForModule(deps: Keys.Classpath, scalaVersion:String, module: ModuleID) = {
    deps.map(_.get(Keys.moduleID.key).get).find { m => equivModule (m, module, scalaVersion) }
  }

  private def libDepFilter(deps: Seq[ModuleID], scalaVersion: String)(module: ModuleID): Boolean = {
    deps.exists(equivModule(_, module, scalaVersion))
  }

  private def libDepFilter(deps: Keys.Classpath)(moduleReport: ModuleReport): Boolean = {
    val exists = deps.files.exists(dep => moduleReport.artifacts.exists(_._2 == dep))
//    println("Exists: %s - %s".format(exists, moduleReport))
    exists
  }

  private def convertDeps(report: UpdateReport, deps: Keys.Classpath, scalaVersion: String, useMavenLibs: Boolean): Seq[(IdeaModuleLibRef, ModuleID)] = {

    //TODO If we could retrieve the correct configurations for the ModuleID, we could filter by configuration in
    //mapToIdeaModuleLibs and remove the hardcoded configurations. Something like the following would be enough:
    //report.configurations.flatMap(configReport => mapToIdeaModuleLibs(configReport.configuration, configReport.modules, deps))

    Seq("compile", "runtime", "test", "provided").flatMap(report.configuration).foldLeft(Seq[(IdeaModuleLibRef, ModuleID)]()) {
      (acc, configReport) =>
        val filteredModules = configReport.modules.filterNot(m1 =>
          acc.exists {
            case (_, m2) => equivModule(m1.module, m2, scalaVersion)
          })
        acc ++ mapToIdeaModuleLibs(configReport.configuration, filteredModules, deps, scalaVersion, useMavenLibs)
    }
  }

  private def addClassifiers(ideaModuleLibRefs: Seq[(IdeaModuleLibRef, ModuleID)],
                             report: UpdateReport, useMavenLibs: Boolean): Seq[(IdeaModuleLibRef, ModuleID)] = {

    /* Both retrieved from UpdateTask, so we don't need to deal with crossVersion here */
    def equivModule(m1: ModuleID, m2: ModuleID): Boolean =
      m1.name == m2.name && m1.organization == m2.organization && m1.revision == m2.revision

    val modifiedModuleLibRefs = {

      report.configurations.flatMap {
        configReport =>

          configReport.modules.flatMap {
            moduleReport =>

              ideaModuleLibRefs.find {
                case (moduleLibRef, moduleId) =>
                  (moduleLibRef.config == toScope(configReport.configuration) || configReport.configuration == "default") && equivModule(moduleReport.module, moduleId)
              } map {
                case (moduleLibRef, moduleId) =>

                  val ideaLibrary = {
                    val il = ideaLibFromModule(moduleReport, useMavenLibs)
                    il.copy(classes = il.classes ++ moduleLibRef.library.classes,
                      javaDocs = il.javaDocs ++ moduleLibRef.library.javaDocs,
                      sources = il.sources ++ moduleLibRef.library.sources,
                      projectDir = moduleLibRef.library.projectDir)
                  }

                  moduleLibRef.copy(library = ideaLibrary) -> moduleId
              }
          }
      }
    }

    val unmodifiedModuleLibRefs = ideaModuleLibRefs.filterNot {
      case (_, m1) =>
        modifiedModuleLibRefs.exists {
          case (_, m2) => equivModule(m1, m2)
        }
    }

    modifiedModuleLibRefs ++ unmodifiedModuleLibRefs

  }

  lazy val localM2Repo = Path.userHome / ".m2/repository"

  def extractLibraries(report: UpdateReport): Seq[IdeaLibrary] = {
    report.configurations.flatMap {
      configReport =>
        configReport.modules.map {
          moduleReport =>
            ideaLibFromModule(moduleReport, false)
        }
    }
  }
}