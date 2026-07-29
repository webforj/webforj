package com.webforj.devtools.craftforj.inspector.source.staging;

import com.github.javaparser.ParseResult;
import com.github.javaparser.Problem;
import com.github.javaparser.ast.CompilationUnit;
import com.webforj.devtools.craftforj.inspector.source.parser.SourceParserService;
import com.webforj.devtools.craftforj.inspector.source.staging.model.CompileDiagnostic;
import com.webforj.devtools.craftforj.inspector.source.staging.model.ValidationResult;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;

/**
 * Validates staged sources against the running application classpath before anything is written.
 *
 * <p>
 * All staged files compile together in one in memory javac task, so a new class and an edited
 * consumer of that class validate as a unit. Before rejecting a failing edit, the unmodified on
 * disk version of each edited file compiles with the same classpath. A file whose baseline already
 * fails, for example under an annotation processor the gate cannot run, degrades to parse only
 * validation and is marked unverified rather than rejected. Without a system compiler every file
 * degrades to parse only validation.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CompileValidator {

  private static final Pattern PACKAGE_PATTERN =
      Pattern.compile("^\\s*package\\s+([\\w.]+)\\s*;", Pattern.MULTILINE);
  private static final Pattern MISSING_SYMBOL_PATTERN =
      Pattern.compile("symbol[\\s:]*class\\s+(\\w+)");
  private static final int MAX_HINTS = 5;

  private final JavaCompiler compiler;
  private final String classpath;
  private final SourceParserService parserService;
  private Map<String, List<String>> simpleNameIndex;

  /**
   * Creates a validator using the system compiler and the running application classpath.
   */
  public CompileValidator() {
    this(ToolProvider.getSystemJavaCompiler(), buildClasspath());
  }

  /**
   * Creates a validator with explicit compiler and classpath.
   *
   * @param compiler the compiler, or {@code null} to force parse only validation
   * @param classpath the classpath handed to the compiler
   */
  public CompileValidator(JavaCompiler compiler, String classpath) {
    this.compiler = compiler;
    this.classpath = classpath;
    this.parserService = SourceParserService.getCurrent();
  }

  /**
   * Checks whether full compile validation is available.
   *
   * @return {@code true} when a system compiler is present
   */
  public boolean isCompileAvailable() {
    return compiler != null;
  }

  /**
   * Validates a set of staged sources as one unit.
   *
   * @param sources staged content keyed by absolute file path
   * @param newFiles the subset of paths that do not exist on disk
   *
   * @return the validation outcome
   */
  public ValidationResult validate(Map<String, String> sources, Set<String> newFiles) {
    if (compiler == null) {
      return parseOnly(sources);
    }

    List<CompileDiagnostic> errors = compile(sources);
    if (errors.isEmpty()) {
      Map<String, Boolean> verified = new HashMap<>();
      sources.keySet().forEach(path -> verified.put(path, true));

      return new ValidationResult(true, verified, List.of());
    }

    Set<String> degraded = findDegradedBaselines(sources, newFiles);
    if (degraded.isEmpty()) {
      return new ValidationResult(false, Map.of(), errors);
    }

    return judgeWithDegraded(sources, errors, degraded);
  }

  private ValidationResult judgeWithDegraded(Map<String, String> sources,
      List<CompileDiagnostic> errors, Set<String> degraded) {
    List<CompileDiagnostic> blocking = new ArrayList<>();
    for (CompileDiagnostic error : errors) {
      if (error.getFile() == null || !degraded.contains(error.getFile())) {
        blocking.add(error);
      }
    }

    for (String path : degraded) {
      blocking.addAll(parse(path, sources.get(path)));
    }

    if (!blocking.isEmpty()) {
      return new ValidationResult(false, Map.of(), blocking);
    }

    Map<String, Boolean> verified = new HashMap<>();
    sources.keySet().forEach(path -> verified.put(path, !degraded.contains(path)));

    return new ValidationResult(true, verified, List.of());
  }

  private ValidationResult parseOnly(Map<String, String> sources) {
    List<CompileDiagnostic> errors = new ArrayList<>();
    Map<String, Boolean> verified = new HashMap<>();
    for (Map.Entry<String, String> entry : sources.entrySet()) {
      errors.addAll(parse(entry.getKey(), entry.getValue()));
      verified.put(entry.getKey(), false);
    }

    if (!errors.isEmpty()) {
      return new ValidationResult(false, Map.of(), errors);
    }

    return new ValidationResult(true, verified, List.of());
  }

  private List<CompileDiagnostic> parse(String path, String content) {
    ParseResult<CompilationUnit> result = parserService.parseWithProblems(content);
    if (result.isSuccessful()) {
      return List.of();
    }

    List<CompileDiagnostic> errors = new ArrayList<>();
    for (Problem problem : result.getProblems()) {
      long line = problem.getLocation().flatMap(range -> range.getBegin().getRange())
          .map(range -> (long) range.begin.line).orElse(-1L);
      long column = problem.getLocation().flatMap(range -> range.getBegin().getRange())
          .map(range -> (long) range.begin.column).orElse(-1L);
      errors.add(new CompileDiagnostic(path, line, column, problem.getMessage(), List.of()));
    }

    return errors;
  }

  private Set<String> findDegradedBaselines(Map<String, String> sources, Set<String> newFiles) {
    Map<String, String> baselines = new LinkedHashMap<>();
    Set<String> degraded = new HashSet<>();
    for (String path : sources.keySet()) {
      if (newFiles.contains(path)) {
        continue;
      }

      try {
        baselines.put(path, Files.readString(Path.of(path), StandardCharsets.UTF_8));
      } catch (IOException e) {
        // Unreadable baseline cannot prove anything about the edit, treat it as degraded
        degraded.add(path);
      }
    }

    if (!baselines.isEmpty()) {
      List<CompileDiagnostic> baselineErrors = compile(baselines);
      for (CompileDiagnostic error : baselineErrors) {
        if (error.getFile() != null) {
          degraded.add(error.getFile());
        }
      }
    }

    return degraded;
  }

  private List<CompileDiagnostic> compile(Map<String, String> sources) {
    DiagnosticCollector<JavaFileObject> collector = new DiagnosticCollector<>();
    Map<URI, String> uriToPath = new HashMap<>();
    List<JavaFileObject> units = new ArrayList<>();
    for (Map.Entry<String, String> entry : sources.entrySet()) {
      InMemorySource unit =
          new InMemorySource(toUnitUri(entry.getKey(), entry.getValue()), entry.getValue());
      uriToPath.put(unit.toUri(), entry.getKey());
      units.add(unit);
    }

    StandardJavaFileManager standard =
        compiler.getStandardFileManager(collector, Locale.ENGLISH, StandardCharsets.UTF_8);
    try (DiscardingFileManager manager = new DiscardingFileManager(standard)) {
      List<String> options = List.of("-proc:none", "-classpath", classpath);
      JavaCompiler.CompilationTask task =
          compiler.getTask(Writer.nullWriter(), manager, collector, options, null, units);
      task.call();
    } catch (IOException e) {
      // Closing the file manager failed after compilation, diagnostics are already collected
    }

    List<CompileDiagnostic> errors = new ArrayList<>();
    for (Diagnostic<? extends JavaFileObject> diagnostic : collector.getDiagnostics()) {
      if (diagnostic.getKind() != Diagnostic.Kind.ERROR) {
        continue;
      }

      String path =
          diagnostic.getSource() == null ? null : uriToPath.get(diagnostic.getSource().toUri());
      String message = diagnostic.getMessage(Locale.ENGLISH);
      errors.add(new CompileDiagnostic(path, diagnostic.getLineNumber(),
          diagnostic.getColumnNumber(), message, hintsFor(message)));
    }

    return errors;
  }

  private List<String> hintsFor(String message) {
    if (message == null || !message.contains("cannot find symbol")) {
      return List.of();
    }

    Matcher matcher = MISSING_SYMBOL_PATTERN.matcher(message);
    if (!matcher.find()) {
      return List.of();
    }

    List<String> matches = getSimpleNameIndex().get(matcher.group(1));
    if (matches == null) {
      return List.of();
    }

    return matches.size() > MAX_HINTS ? matches.subList(0, MAX_HINTS) : matches;
  }

  private synchronized Map<String, List<String>> getSimpleNameIndex() {
    if (simpleNameIndex == null) {
      simpleNameIndex = buildSimpleNameIndex(classpath);
    }

    return simpleNameIndex;
  }

  private static Map<String, List<String>> buildSimpleNameIndex(String classpath) {
    Map<String, List<String>> index = new HashMap<>();
    for (String entry : classpath.split(File.pathSeparator)) {
      File file = new File(entry);
      if (file.isFile() && entry.endsWith(".jar")) {
        indexJar(file, index);
      } else if (file.isDirectory()) {
        indexDirectory(file.toPath(), index);
      }
    }

    return index;
  }

  private static void indexJar(File jar, Map<String, List<String>> index) {
    try (JarFile jarFile = new JarFile(jar)) {
      Enumeration<JarEntry> entries = jarFile.entries();
      while (entries.hasMoreElements()) {
        addClassEntry(entries.nextElement().getName(), index);
      }
    } catch (IOException e) {
      // A broken jar on the classpath only costs its hints
    }
  }

  private static void indexDirectory(Path root, Map<String, List<String>> index) {
    try (var paths = Files.walk(root)) {
      paths.filter(Files::isRegularFile)
          .forEach(path -> addClassEntry(root.relativize(path).toString(), index));
    } catch (IOException e) {
      // An unreadable directory on the classpath only costs its hints
    }
  }

  private static void addClassEntry(String entryName, Map<String, List<String>> index) {
    if (!entryName.endsWith(".class") || entryName.contains("$")
        || entryName.equals("module-info.class")) {
      return;
    }

    String normalized = entryName.replace(File.separatorChar, '/');
    String className =
        normalized.substring(0, normalized.length() - ".class".length()).replace('/', '.');
    String simpleName = className.substring(className.lastIndexOf('.') + 1);
    index.computeIfAbsent(simpleName, ignored -> new ArrayList<>()).add(className);
  }

  private static URI toUnitUri(String path, String content) {
    String fileName = Path.of(path).getFileName().toString();
    String packagePath = "";
    if (content != null) {
      Matcher matcher = PACKAGE_PATTERN.matcher(content);
      if (matcher.find()) {
        packagePath = matcher.group(1).replace('.', '/') + "/";
      }
    }

    return URI.create("string:///" + packagePath + fileName);
  }

  private static String buildClasspath() {
    Set<String> entries = new LinkedHashSet<>();
    String systemClasspath = System.getProperty("java.class.path", "");
    for (String entry : systemClasspath.split(File.pathSeparator)) {
      if (!entry.isEmpty()) {
        entries.add(entry);
      }
    }

    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    while (loader != null) {
      if (loader instanceof URLClassLoader urlLoader) {
        for (URL url : urlLoader.getURLs()) {
          try {
            entries.add(new File(url.toURI()).getAbsolutePath());
          } catch (Exception e) {
            // Non file URLs cannot join a javac classpath
          }
        }
      }
      loader = loader.getParent();
    }

    return String.join(File.pathSeparator, entries);
  }

  private static final class InMemorySource extends SimpleJavaFileObject {

    private final String content;

    InMemorySource(URI uri, String content) {
      super(uri, Kind.SOURCE);
      this.content = content == null ? "" : content;
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors) {
      return content;
    }
  }

  private static final class InMemoryOutput extends SimpleJavaFileObject {

    InMemoryOutput(String className, Kind kind) {
      super(URI.create("mem:///" + className.replace('.', '/') + kind.extension), kind);
    }

    @Override
    public OutputStream openOutputStream() {
      return OutputStream.nullOutputStream();
    }
  }

  private static final class DiscardingFileManager
      extends ForwardingJavaFileManager<StandardJavaFileManager> {

    DiscardingFileManager(StandardJavaFileManager delegate) {
      super(delegate);
    }

    @Override
    public JavaFileObject getJavaFileForOutput(Location location, String className,
        JavaFileObject.Kind kind, FileObject sibling) {
      return new InMemoryOutput(className, kind);
    }

    @Override
    public boolean hasLocation(Location location) {
      if (location == StandardLocation.CLASS_OUTPUT) {
        return true;
      }

      return super.hasLocation(location);
    }
  }
}
