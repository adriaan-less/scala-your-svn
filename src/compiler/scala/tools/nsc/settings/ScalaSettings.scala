/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools
package nsc
package settings

import annotation.elidable
import scala.tools.util.PathResolver.Defaults
import scala.collection.mutable

trait ScalaSettings extends AbsScalaSettings
                       with StandardScalaSettings
                       with Warnings {
  self: MutableSettings =>

  /** Set of settings */
  protected lazy val allSettings = mutable.HashSet[Setting]()

  /** Against my better judgment, giving in to martin here and allowing
   *  CLASSPATH to be used automatically.  So for the user-specified part
   *  of the classpath:
   *
   *  - If -classpath or -cp is given, it is that
   *  - Otherwise, if CLASSPATH is set, it is that
   *  - If neither of those, then "." is used.
   */
  protected def defaultClasspath = sys.env.getOrElse("CLASSPATH", ".")

  /** Disable a setting */
  def disable(s: Setting) = allSettings -= s

  val jvmargs  = PrefixSetting("-J<flag>", "-J", "Pass <flag> directly to the runtime system.")
  val defines  = PrefixSetting("-Dproperty=value", "-D", "Pass -Dproperty=value directly to the runtime system.")
  val toolcp   = PathSetting("-toolcp", "Add to the runner classpath.", "")
  val nobootcp = BooleanSetting("-nobootcp", "Do not use the boot classpath for the scala jars.")

  /**
   *  Standard settings
   */
  // argfiles is only for the help message
  val argfiles      = BooleanSetting    ("@<file>", "A text file containing compiler arguments (options and source files)")
  val classpath     = PathSetting       ("-classpath", "Specify where to find user class files.", defaultClasspath) .
                                            withAbbreviation ("-cp")
  val d             = OutputSetting     (outputDirs, ".")
  val optimise      = BooleanSetting    ("-optimise", "Generates faster bytecode by applying optimisations to the program") .
                                            withAbbreviation("-optimize") .
                                            withPostSetHook(set => List(inline, inlineHandlers, Xcloselim, Xdce) foreach (_.value = set.value))
  val nospecialization = BooleanSetting    ("-no-specialization", "Ignore @specialize annotations.")

  /**
   * -X "Advanced" settings
   */
  val Xhelp         = BooleanSetting    ("-X", "Print a synopsis of advanced options")
  val assemname     = StringSetting     ("-Xassem-name", "file", "Name of the output assembly (only relevant with -target:msil)", "").dependsOn(target, "msil")
  val assemrefs     = StringSetting     ("-Xassem-path", "path", "List of assemblies referenced by the program (only relevant with -target:msil)", ".").dependsOn(target, "msil")
  val assemextdirs  = StringSetting     ("-Xassem-extdirs", "dirs", "List of directories containing assemblies, defaults to `lib'", Defaults.scalaLibDir.path).dependsOn(target, "msil")
  val sourcedir     = StringSetting     ("-Xsourcedir", "directory", "When -target:msil, the source folder structure is mirrored in output directory.", ".").dependsOn(target, "msil")
  val checkInit     = BooleanSetting    ("-Xcheckinit", "Add runtime checks on field accessors. Uninitialized accesses result in an exception being thrown.")
  val noassertions  = BooleanSetting    ("-Xdisable-assertions", "Generate no assertions and assumptions")
  val elidebelow    = IntSetting        ("-Xelide-below", "Generate calls to @elidable-marked methods only if method priority is greater than argument.",
                                                elidable.ASSERTION, None, elidable.byName.get(_))
  val noForwarders  = BooleanSetting    ("-Xno-forwarders", "Do not generate static forwarders in mirror classes")
  val future        = BooleanSetting    ("-Xfuture", "Turn on future language features")
  val genPhaseGraph = StringSetting     ("-Xgenerate-phase-graph", "file", "Generate the phase graphs (outputs .dot files) to fileX.dot", "")
  val XlogImplicits = BooleanSetting    ("-Xlog-implicits", "Show more info on why some implicits are not applicable")
  val Xmigration28  = BooleanSetting    ("-Xmigration", "Warn about constructs whose behavior may have changed between 2.7 and 2.8")
  val nouescape     = BooleanSetting    ("-Xno-uescape", "Disables handling of \\u unicode escapes")
  val Xnojline      = BooleanSetting    ("-Xnojline", "Do not use JLine for editing")
  val plugin        = MultiStringSetting("-Xplugin", "file", "Load a plugin from a file")
  val disable       = MultiStringSetting("-Xplugin-disable", "plugin", "Disable a plugin")
  val showPlugins   = BooleanSetting    ("-Xplugin-list", "Print a synopsis of loaded plugins")
  val require       = MultiStringSetting("-Xplugin-require", "plugin", "Abort unless a plugin is available")
  val pluginsDir    = StringSetting     ("-Xpluginsdir", "path", "Path to search compiler plugins", Defaults.scalaPluginPath)
  val Xprint        = PhasesSetting     ("-Xprint", "Print out program after")
  val writeICode    = BooleanSetting    ("-Xprint-icode", "Log internal icode to *.icode files")
  val Xprintpos     = BooleanSetting    ("-Xprint-pos", "Print tree positions (as offsets)")
  val printtypes    = BooleanSetting    ("-Xprint-types", "Print tree types (debugging option)")
  val prompt        = BooleanSetting    ("-Xprompt", "Display a prompt after each error (debugging option)")
  val resident      = BooleanSetting    ("-Xresident", "Compiler stays resident, files to compile are read from standard input")
  val script        = StringSetting     ("-Xscript", "object", "Compile as a script, wrapping the code into object.main()", "")
  val Xshowcls      = StringSetting     ("-Xshow-class", "class", "Show class info", "")
  val Xshowobj      = StringSetting     ("-Xshow-object", "object", "Show object info", "")
  val showPhases    = BooleanSetting    ("-Xshow-phases", "Print a synopsis of compiler phases")
  val sourceReader  = StringSetting     ("-Xsource-reader", "classname", "Specify a custom method for reading source files", "scala.tools.nsc.io.SourceReader")

  val Xwarnfatal    = BooleanSetting    ("-Xfatal-warnings", "Fail the compilation if there are any warnings.")
  val Xwarninit     = BooleanSetting    ("-Xwarninit", "Warn about possible changes in initialization semantics")
  val Xchecknull    = BooleanSetting    ("-Xcheck-null", "Emit warning on selection of nullable reference")

  // Experimental Extensions
  val Xexperimental = BooleanSetting    ("-Xexperimental", "Enable experimental extensions") .
                          withPostSetHook(_ => List(YdepMethTpes, YmethodInfer) foreach (_.value = true)) //YvirtClasses, 
  val YdepMethTpes  = BooleanSetting    ("-Ydependent-method-types", "Allow dependent method types")
  val YmethodInfer  = BooleanSetting    ("-Yinfer-argument-types", "Infer types for arguments of overriden methods")
  val YvirtClasses  = false // too embryonic to even expose as a -Y //BooleanSetting    ("-Yvirtual-classes", "Support virtual classes")

  /** Compatibility stubs for options whose value name did
   *  not previously match the option name.
   */
  def XO = optimise
  def debuginfo = g
  def dependenciesFile = dependencyfile
  def nowarnings = nowarn
  def outdir = d
  def printLate = print

  /**
   * -Y "Private" settings
   */
  val overrideObjects = BooleanSetting ("-Yoverride-objects", "Allow member objects to be overridden.")
  val Yhelp         = BooleanSetting    ("-Y", "Print a synopsis of private options.")
  val browse        = PhasesSetting     ("-Ybrowse", "Browse the abstract syntax tree after")
  val check         = PhasesSetting     ("-Ycheck", "Check the tree at the end of")
  val Yshow         = PhasesSetting     ("-Yshow", "(Requires -Xshow-class or -Xshow-object) Show after")
  val Xcloselim     = BooleanSetting    ("-Yclosure-elim", "Perform closure elimination.")
  val Ycompacttrees = BooleanSetting    ("-Ycompact-trees", "Use compact tree printer when displaying trees.")
  val noCompletion  = BooleanSetting    ("-Yno-completion", "Disable tab-completion in the REPL.")
  val Xdce          = BooleanSetting    ("-Ydead-code", "Perform dead code elimination.")
  val debug         = BooleanSetting    ("-Ydebug", "Increase the quantity of debugging output.")
  // val doc           = BooleanSetting    ("-Ydoc", "Generate documentation")
  val termConflict  = ChoiceSetting     ("-Yresolve-term-conflict", "strategy", "Resolve term conflicts",
    List("package", "object", "error"), "error")
  val inline        = BooleanSetting    ("-Yinline", "Perform inlining when possible.")
  val inlineHandlers= BooleanSetting    ("-Yinline-handlers", "Perform exception handler inlining when possible.")
  val Xlinearizer   = ChoiceSetting     ("-Ylinearizer", "which", "Linearizer to use", List("normal", "dfs", "rpo", "dump"), "rpo")
  val log           = PhasesSetting     ("-Ylog", "Log operations during")
  val Ylogcp        = BooleanSetting    ("-Ylog-classpath", "Output information about what classpath is being applied.")
  val Ynogenericsig = BooleanSetting    ("-Yno-generic-signatures", "Suppress generation of generic signatures for Java.")
  val noimports     = BooleanSetting    ("-Yno-imports", "Compile without importing scala.*, java.lang.*, or Predef.")
  val nopredef      = BooleanSetting    ("-Yno-predef", "Compile without importing Predef.")
  val noAdaptedArgs = BooleanSetting    ("-Yno-adapted-args", "Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.")
  val Yprofile      = PhasesSetting     ("-Yprofile", "(Requires jvm -agentpath to contain yjgpagent) Profile CPU usage of")
  val YprofileMem   = BooleanSetting    ("-Yprofile-memory", "Profile memory, get heap snapshot after each compiler run (requires yjpagent, see above).")
  val YprofileClass = StringSetting     ("-Yprofile-class", "class", "Name of profiler class.", "scala.tools.util.YourkitProfiling")
  val Yrecursion    = IntSetting        ("-Yrecursion", "Set recursion depth used when locking symbols.", 0, Some((0, Int.MaxValue)), (_: String) => None)
  val selfInAnnots  = BooleanSetting    ("-Yself-in-annots", "Include a \"self\" identifier inside of annotations.")
  val Xshowtrees    = BooleanSetting    ("-Yshow-trees", "(Requires -Xprint:) Print detailed ASTs.")
  val Yshowsyms     = BooleanSetting    ("-Yshow-syms", "Print the AST symbol hierarchy after each phase.")
  val Yshowsymkinds = BooleanSetting    ("-Yshow-symkinds", "Print abbreviated symbol kinds next to symbol names.")
  val skip          = PhasesSetting     ("-Yskip", "Skip")
  val Ygenjavap     = StringSetting     ("-Ygen-javap", "dir", "Generate a parallel output directory of .javap files.", "")
  val Ydumpclasses  = StringSetting     ("-Ydump-classes", "dir", "Dump the generated bytecode to .class files (useful for reflective compilation that utilizes in-memory classloaders).", "")
  val Ynosqueeze    = BooleanSetting    ("-Yno-squeeze", "Disable creation of compact code in matching.")
  val Ystatistics   = BooleanSetting    ("-Ystatistics", "Print compiler statistics.") .
                                          withPostSetHook(set => util.Statistics.enabled = set.value)
  val stopAfter     = PhasesSetting     ("-Ystop-after", "Stop after") withAbbreviation ("-stop") // backward compat
  val stopBefore    = PhasesSetting     ("-Ystop-before", "Stop before")
  val refinementMethodDispatch =
                      ChoiceSetting     ("-Ystruct-dispatch", "policy", "structural method dispatch policy",
                        List("no-cache", "mono-cache", "poly-cache", "invoke-dynamic"), "poly-cache")
  val globalClass   = StringSetting     ("-Yglobal-class", "class", "subclass of scala.tools.nsc.Global to use for compiler", "")
  val Yrangepos     = BooleanSetting    ("-Yrangepos", "Use range positions for syntax trees.")
  val YrichExes     = BooleanSetting    ("-Yrich-exceptions",
                                            "Fancier exceptions.  Set source search path with -D" +
                                            sys.SystemProperties.traceSourcePath.key)
  val Yidedebug     = BooleanSetting    ("-Yide-debug", "Generate, validate and output trees using the interactive compiler.")
  val Ybuilderdebug = ChoiceSetting     ("-Ybuilder-debug", "manager", "Compile using the specified build manager.", List("none", "refined", "simple"), "none")
  val Ybuildmanagerdebug =
                      BooleanSetting    ("-Ybuild-manager-debug", "Generate debug information for the Refined Build Manager compiler.")
  val Ytyperdebug   = BooleanSetting    ("-Ytyper-debug", "Trace all type assignments.")
  val Yinferdebug   = BooleanSetting    ("-Yinfer-debug", "Trace type inference and implicit search.")
  val Ypmatdebug    = BooleanSetting    ("-Ypmat-debug", "Trace all pattern matcher activity.")
  val Yreifycopypaste =
                      BooleanSetting    ("-Yreify-copypaste", "Dump the reified trees in copypasteable representation.")
  val Yreifydebug   = BooleanSetting    ("-Yreify-debug", "Trace reification.")
  val Ymacrodebug   = BooleanSetting    ("-Ymacro-debug", "Trace macro-related activities: generation of synthetics, expansion, exceptions.")
  val Yreplsync     = BooleanSetting    ("-Yrepl-sync", "Do not use asynchronous code for repl startup")
  val Yrepldebug    = BooleanSetting    ("-Yrepl-debug", "Trace all repl activity.") .
                                          withPostSetHook(_ => interpreter.replProps.debug setValue true)
  val Ycompletion   = BooleanSetting    ("-Ycompletion-debug", "Trace all tab completion activity.")
  val Ydocdebug     = BooleanSetting    ("-Ydoc-debug", "Trace all scaladoc activity.")
  val Ypmatnaive    = BooleanSetting    ("-Ypmat-naive", "Desugar matches as naively as possible.")
  val Ynotnull      = BooleanSetting    ("-Ynotnull", "Enable (experimental and incomplete) scala.NotNull.")
  // val YdepMethTpes  = BooleanSetting    ("-Ydependent-method-types", "Allow dependent method types.")
  val YmethodInfer  = BooleanSetting    ("-Yinfer-argument-types", "Infer types for arguments of overriden methods.")
  val etaExpandKeepsStar = BooleanSetting("-Yeta-expand-keeps-star", "Eta-expand varargs methods to T* rather than Seq[T].  This is a temporary option to ease transition.")
  val noSelfCheck   = BooleanSetting    ("-Yno-self-type-checks", "Suppress check for self-type conformance among inherited members.")
  val YvirtPatmat   = BooleanSetting    ("-Yvirtpatmat", "Translate pattern matches into flatMap/orElse calls. See scala.MatchingStrategy.")
  val YvirtClasses  = false // too embryonic to even expose as a -Y //BooleanSetting    ("-Yvirtual-classes", "Support virtual classes")

  val exposeEmptyPackage = BooleanSetting("-Yexpose-empty-package", "Internal only: expose the empty package.").internalOnly()
  val YnoProductN = BooleanSetting ("-Yno-productN", "Do not add ProductN to case classes")

  def stop = stopAfter

  /**
   * IDE-specific settings
   */
  val YpresentationVerbose = BooleanSetting("-Ypresentation-verbose", "Print information about presentation compiler tasks.")
  val YpresentationDebug   = BooleanSetting("-Ypresentation-debug",  "Enable debugging output for the presentation compiler.")
  val YpresentationStrict  = BooleanSetting("-Ypresentation-strict", "Do not report type errors in sources with syntax errors.")

  val YpresentationLog     = StringSetting("-Ypresentation-log", "file", "Log presentation compiler events into file", "")
  val YpresentationReplay  = StringSetting("-Ypresentation-replay", "file", "Replay presentation compiler events from file", "")
  val YpresentationDelay   = IntSetting("-Ypresentation-delay", "Wait number of ms after typing before starting typechecking", 0, Some((0, 999)), str => Some(str.toInt))

  /**
   * -P "Plugin" settings
   */
  val pluginOptions = MultiStringSetting("-P", "plugin:opt", "Pass an option to a plugin") .
                        withHelpSyntax("-P:<plugin>:<opt>")
}
