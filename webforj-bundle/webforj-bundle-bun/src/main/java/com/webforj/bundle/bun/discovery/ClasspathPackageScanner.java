package com.webforj.bundle.bun.discovery;

import com.webforj.bundle.annotation.BundleEntry;
import com.webforj.bundle.annotation.BundlePackage;
import io.github.classgraph.AnnotationInfo;
import io.github.classgraph.AnnotationParameterValueList;
import io.github.classgraph.ClassGraph;
import io.github.classgraph.ClassInfo;
import io.github.classgraph.ScanResult;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Scans a set of classpath URLs for {@link BundlePackage} and {@link BundleEntry} packages.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class ClasspathPackageScanner {

  /**
   * Package prefixes that never declare {@code @BundleEntry} or {@code @BundlePackage}, skipped so
   * the scan does not parse the class files of the platform, the build tooling, and common
   * libraries.
   */
  private static final String[] EXCLUDED_PACKAGES = {"java", "javax", "jakarta", "javafx", "jdk",
      "sun", "com.sun", "oracle", "com.basis", "kotlin", "scala", "groovy", "antlr", "cglib",
      "junit", "net.bytebuddy", "ch.qos", "com.fasterxml", "com.google", "com.h2database",
      "com.helger", "com.intellij", "com.zaxxer", "org.apache", "org.aspectj", "org.bouncycastle",
      "org.dom4j", "org.easymock", "org.eclipse", "org.hamcrest", "org.hibernate", "org.javassist",
      "org.jboss", "org.jsoup", "org.seleniumhq", "org.slf4j", "org.atmosphere",
      "org.springframework", "org.yaml", "org.jetbrains", "org.junit", "org.mockito",
      "org.objectweb", "org.reactivestreams", "org.checkerframework", "io.micrometer", "io.netty",
      "io.github.classgraph"};

  /**
   * The result of a scan.
   */
  public static class Result {

    private List<BundlePackageDeclaration> packages = null;
    private Set<String> sources = null;
    private Set<String> debugSources = Set.of();
    private Map<String, Set<String>> bindings = Map.of();
    private List<String> warnings = null;

    /**
     * Sets the deduplicated package packages.
     *
     * @param packages the deduplicated package packages
     * @return the result
     */
    public Result setPackages(List<BundlePackageDeclaration> packages) {
      this.packages = packages;

      return this;
    }

    /**
     * Gets the deduplicated package packages.
     *
     * @return the deduplicated package packages
     * @see #setPackages(List)
     */
    public List<BundlePackageDeclaration> getPackages() {
      return packages;
    }

    /**
     * Sets the declared entry names.
     *
     * @param sources the declared entry names
     * @return the result
     */
    public Result setSources(Set<String> sources) {
      this.sources = sources;

      return this;
    }

    /**
     * Gets the declared entry names.
     *
     * @return the declared entry names
     * @see #setSources(Set)
     */
    public Set<String> getSources() {
      return sources;
    }

    /**
     * Sets the entry names declared with {@code debug = true}.
     *
     * @param debugSources the debug only entry names
     * @return the result
     */
    public Result setDebugSources(Set<String> debugSources) {
      this.debugSources = debugSources;

      return this;
    }

    /**
     * Gets the entry names declared with {@code debug = true}, injected only in debug mode.
     *
     * @return the debug only entry names
     * @see #setDebugSources(Set)
     */
    public Set<String> getDebugSources() {
      return debugSources;
    }

    /**
     * Sets the binding of each class to the entry names it declares.
     *
     * @param bindings the class to declared entry names binding
     * @return the result
     */
    public Result setBindings(Map<String, Set<String>> bindings) {
      this.bindings = bindings;

      return this;
    }

    /**
     * Gets the binding of each declaring class name to the entry names it declares through
     * {@code @BundleEntry}.
     *
     * @return the class to declared entry names binding
     * @see #setBindings(Map)
     */
    public Map<String, Set<String>> getBindings() {
      return bindings;
    }

    /**
     * Sets the warnings produced while resolving conflicts.
     *
     * @param warnings the warnings
     * @return the result
     */
    public Result setWarnings(List<String> warnings) {
      this.warnings = warnings;

      return this;
    }

    /**
     * Gets the warnings produced while resolving conflicts.
     *
     * @return the warnings
     * @see #setWarnings(List)
     */
    public List<String> getWarnings() {
      return warnings;
    }
  }

  /**
   * Scans the given classpath roots for {@code @BundlePackage} and {@code @BundleEntry} packages.
   *
   * @param classpathRoots the URIs to scan (compiled output, JAR dependencies)
   * @return the scan result
   */
  public Result scan(Set<URI> classpathRoots) {
    return scan(classpathRoots, Set.of());
  }

  /**
   * Scans the given classpath roots for {@code @BundlePackage} and {@code @BundleEntry} packages,
   * skipping the built in exclusions plus the supplied extra package prefixes.
   *
   * @param classpathRoots the URIs to scan (compiled output, JAR dependencies)
   * @param additionalExcludedPackages extra package prefixes to skip, supplied by the project
   * @return the scan result
   */
  public Result scan(Set<URI> classpathRoots, Collection<String> additionalExcludedPackages) {
    List<BundlePackageDeclaration> raw = new ArrayList<>();
    Set<String> sources = new LinkedHashSet<>();
    Set<String> debugSources = new LinkedHashSet<>();
    Map<String, Set<String>> bindings = new LinkedHashMap<>();
    ClassGraph graph = new ClassGraph().enableClassInfo().enableAnnotationInfo()
        .rejectPackages(excludedPackages(additionalExcludedPackages))
        .overrideClasspath(classpathRoots);

    try (ScanResult scan = graph.scan()) {
      collectPackages(scan, raw);
      collectSources(scan, sources, debugSources, bindings);
    }

    Result merged = merge(raw);

    return new Result().setPackages(merged.getPackages()).setSources(sources)
        .setDebugSources(debugSources).setBindings(bindings).setWarnings(merged.getWarnings());
  }

  private static String[] excludedPackages(Collection<String> additional) {
    if (additional == null || additional.isEmpty()) {
      return EXCLUDED_PACKAGES;
    }

    List<String> all = new ArrayList<>(Arrays.asList(EXCLUDED_PACKAGES));
    for (String prefix : additional) {
      String trimmed = prefix == null ? "" : prefix.trim();
      if (!trimmed.isEmpty()) {
        all.add(trimmed);
      }
    }

    return all.toArray(String[]::new);
  }

  private static void collectPackages(ScanResult scan, List<BundlePackageDeclaration> raw) {
    for (ClassInfo cls : scan.getClassesWithAnnotation(BundlePackage.class.getName())) {
      for (AnnotationInfo ann : cls.getAnnotationInfo()) {
        if (BundlePackage.class.getName().equals(ann.getName())) {
          addDeclaration(ann, raw);
        }
      }
    }

    for (ClassInfo cls : scan.getClassesWithAnnotation(BundlePackage.Container.class.getName())) {
      for (AnnotationInfo container : cls.getAnnotationInfo()) {
        if (BundlePackage.Container.class.getName().equals(container.getName())) {
          Object nested = container.getParameterValues().getValue("value");
          if (nested instanceof Object[] array) {
            for (Object element : array) {
              if (element instanceof AnnotationInfo nestedAnn) {
                addDeclaration(nestedAnn, raw);
              }
            }
          }
        }
      }
    }
  }

  private static void addDeclaration(AnnotationInfo ann, List<BundlePackageDeclaration> raw) {
    BundlePackageDeclaration parsed = createDeclaration(ann);
    if (parsed != null) {
      raw.add(parsed);
    }
  }

  private static void collectSources(ScanResult scan, Set<String> sources, Set<String> debugSources,
      Map<String, Set<String>> bindings) {
    for (ClassInfo cls : scan.getClassesWithAnnotation(BundleEntry.class.getName())) {
      for (AnnotationInfo ann : cls.getAnnotationInfo()) {
        if (BundleEntry.class.getName().equals(ann.getName())) {
          collectEntry(ann, cls.getName(), sources, debugSources, bindings);
        }
      }
    }

    for (ClassInfo cls : scan.getClassesWithAnnotation(BundleEntry.Container.class.getName())) {
      for (AnnotationInfo container : cls.getAnnotationInfo()) {
        if (BundleEntry.Container.class.getName().equals(container.getName())) {
          Object nested = container.getParameterValues().getValue("value");
          if (nested instanceof Object[] array) {
            for (Object element : array) {
              if (element instanceof AnnotationInfo nestedAnn) {
                collectEntry(nestedAnn, cls.getName(), sources, debugSources, bindings);
              }
            }
          }
        }
      }
    }
  }

  private static void collectEntry(AnnotationInfo entry, String className, Set<String> sources,
      Set<String> debugSources, Map<String, Set<String>> bindings) {
    Object value = entry.getParameterValues().getValue("value");
    if (value == null) {
      return;
    }

    String source = value.toString();
    sources.add(source);
    bind(bindings, className, source);

    if (Boolean.TRUE.equals(entry.getParameterValues().getValue("debug"))) {
      debugSources.add(source);
    }
  }

  private static void bind(Map<String, Set<String>> bindings, String className, String entryName) {
    bindings.computeIfAbsent(className, key -> new LinkedHashSet<>()).add(entryName);
  }

  static Result merge(List<BundlePackageDeclaration> incoming) {
    Map<String, BundlePackageDeclaration> packages = new LinkedHashMap<>();
    Set<String> warnings = new LinkedHashSet<>();

    for (BundlePackageDeclaration decl : incoming) {
      BundlePackageDeclaration existing = packages.putIfAbsent(decl.getName(), decl);

      if (existing != null && !existing.getVersion().equals(decl.getVersion())) {
        warnings.add(String.format(
            "duplicate @BundlePackage for '%s' with different versions ('%s' vs '%s'). Keeping '%s'.",
            decl.getName(), existing.getVersion(), decl.getVersion(), existing.getVersion()));
      }
    }

    return new Result().setPackages(List.copyOf(packages.values())).setSources(Set.of())
        .setWarnings(List.copyOf(warnings));
  }

  private static BundlePackageDeclaration createDeclaration(AnnotationInfo ann) {
    AnnotationParameterValueList params = ann.getParameterValues();
    Object name = params.getValue("value");
    Object version = params.getValue("version");

    if (name == null || version == null) {
      return null;
    }

    Object dev = params.getValue("dev");
    boolean isDev = dev instanceof Boolean bool && bool;

    return new BundlePackageDeclaration().setName(name.toString()).setVersion(version.toString())
        .setDev(isDev);
  }
}
