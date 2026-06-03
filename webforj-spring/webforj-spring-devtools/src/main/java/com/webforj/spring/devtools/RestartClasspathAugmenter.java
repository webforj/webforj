package com.webforj.spring.devtools;

import java.io.File;
import java.io.InputStream;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.SpringApplicationRunListener;
import org.springframework.boot.bootstrap.ConfigurableBootstrapContext;
import org.springframework.boot.devtools.restart.AgentReloader;
import org.springframework.boot.devtools.restart.DefaultRestartInitializer;
import org.springframework.boot.devtools.restart.Restarter;
import org.springframework.core.Ordered;

/**
 * Seeds dependency JARs that depend on webforJ onto the Spring DevTools restart classloader, so a
 * dependency that ships webforJ UI is loaded alongside the framework and the application instead of
 * on the base classloader.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class RestartClasspathAugmenter implements SpringApplicationRunListener, Ordered {
  private static final System.Logger logger =
      System.getLogger(RestartClasspathAugmenter.class.getName());

  /**
   * JVM wide marker so the detection runs once, even though the listener is created on every
   * restart.
   */
  private static final String APPLIED_PROPERTY = "webforj.devtools.restart.classpath-augmented";
  private static final String RESTART_ENABLED_PROPERTY = "spring.devtools.restart.enabled";
  private static final Pattern WEBFORJ_JAR = Pattern.compile("webforj-[\\w\\d-.]+\\.jar");

  /**
   * Matches a {@code com.webforj} groupId, the webforJ group or one of its subgroups, inside a
   * Maven descriptor, without matching an unrelated group such as {@code com.webforjx}.
   */
  private static final Pattern WEBFORJ_GROUP_ID =
      Pattern.compile("<groupId>\\s*com\\.webforj(\\.[\\w.-]+)?\\s*</groupId>");

  private final String[] args;

  /**
   * Creates the run listener. This constructor is used by Spring Boot.
   *
   * @param application the application being started
   * @param args the application arguments
   */
  public RestartClasspathAugmenter(SpringApplication application, String[] args) {
    this.args = args.clone();
  }

  RestartClasspathAugmenter() {
    this.args = new String[0];
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public int getOrder() {
    // Run before the event publishing run listener (order 0) that fires the starting event, so this
    // initializes the Restarter before RestartApplicationListener does.
    return Ordered.HIGHEST_PRECEDENCE;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void starting(ConfigurableBootstrapContext bootstrapContext) {
    if (isApplied()) {
      return;
    }

    markAsApplied();

    // When restart is disabled there is no classloader split, so a webforJ dependency is already on
    // the same classloader as webforJ and needs nothing.
    if (!isRestartEnabled()) {
      return;
    }

    long startNanos = System.nanoTime();
    List<URL> additions = findWebforjDependentJars(resolveClasspathEntries());
    long elapsedMillis = (System.nanoTime() - startNanos) / 1_000_000;

    logger.log(System.Logger.Level.INFO,
        "Classpath detection found {0} webforJ dependent JAR(s) in {1} ms", additions.size(),
        elapsedMillis);

    if (additions.isEmpty()) {
      return;
    }

    logger.log(System.Logger.Level.INFO,
        "Seeding {0} webforJ dependent JAR(s) into the restart classloader", additions.size());
    Restarter.initialize(args, false, new AdditionalUrlsRestartInitializer(additions), true);
  }

  private boolean isApplied() {
    return System.getProperty(APPLIED_PROPERTY) != null;
  }

  private void markAsApplied() {
    System.setProperty(APPLIED_PROPERTY, "true");
  }

  private boolean isRestartEnabled() {
    String property = System.getProperty(RESTART_ENABLED_PROPERTY);
    if (property != null) {
      return Boolean.parseBoolean(property);
    }

    return !AgentReloader.isActive();
  }

  private Set<File> resolveClasspathEntries() {
    // URLs are taken from the classloader hierarchy, which covers launchers whose dependencies are
    // not on {@code java.class.path} such as the Spring Boot JAR launcher, and merged with
    // {@code java.class.path}, which covers the standard application classloader that does not
    // expose its URLs.

    Set<File> files = new LinkedHashSet<>();

    ClassLoader loader = Thread.currentThread().getContextClassLoader();
    if (loader == null) {
      loader = getClass().getClassLoader();
    }

    while (loader != null) {
      if (loader instanceof URLClassLoader urlLoader) {
        for (URL url : urlLoader.getURLs()) {
          if ("file".equals(url.getProtocol())) {
            try {
              files.add(new File(url.toURI()).getAbsoluteFile());
            } catch (Exception e) {
              logger.log(System.Logger.Level.DEBUG, "Could not resolve classpath URL " + url, e);
            }
          }
        }
      }

      loader = loader.getParent();
    }

    for (String entry : System.getProperty("java.class.path", "").split(File.pathSeparator)) {
      if (!entry.isEmpty()) {
        files.add(new File(entry).getAbsoluteFile());
      }
    }

    return files;
  }

  List<URL> findWebforjDependentJars(Collection<File> entries) {
    List<URL> result = new ArrayList<>();

    for (File entry : entries) {
      URL url = resolveWebforjDependentJarUrl(entry);
      if (url != null) {
        result.add(url);
      }
    }

    return result;
  }

  private URL resolveWebforjDependentJarUrl(File jar) {
    String name = jar.getName();
    if (!name.endsWith(".jar") || WEBFORJ_JAR.matcher(name).matches()) {
      return null;
    }

    if (!jar.isFile() || !isWebforjDependent(jar)) {
      return null;
    }

    try {
      return jar.toURI().toURL();
    } catch (Exception e) {
      logger.log(System.Logger.Level.DEBUG, "Could not resolve URL for " + jar, e);

      return null;
    }
  }

  private boolean isWebforjDependent(File jar) {
    try (ZipFile zip = new ZipFile(jar)) {
      Enumeration<? extends ZipEntry> entries = zip.entries();

      while (entries.hasMoreElements()) {
        ZipEntry zipEntry = entries.nextElement();
        String name = zipEntry.getName();

        if (name.startsWith("META-INF/maven/") && name.endsWith("/pom.xml")) {
          try (InputStream in = zip.getInputStream(zipEntry)) {
            String pom = new String(in.readAllBytes(), StandardCharsets.UTF_8);

            if (WEBFORJ_GROUP_ID.matcher(pom).find()) {
              return true;
            }
          }
        }
      }
    } catch (Exception e) {
      logger.log(System.Logger.Level.DEBUG, "Could not read descriptor of " + jar, e);
    }

    return false;
  }

  /**
   * A restart initializer that appends additional JAR URLs to the initial restart classloader.
   *
   * <p>
   * It delegates the development context decision to {@link DefaultRestartInitializer}. When the
   * delegate returns no URLs the application is not in a development context, so no URLs are added
   * and restart stays disabled.
   * </p>
   */
  private static final class AdditionalUrlsRestartInitializer extends DefaultRestartInitializer {

    private final List<URL> additional;

    AdditionalUrlsRestartInitializer(List<URL> additional) {
      this.additional = additional;
    }

    @Override
    public URL[] getInitialUrls(Thread thread) {
      URL[] base = super.getInitialUrls(thread);
      if (base == null) {
        return null;
      }

      List<URL> merged = new ArrayList<>(Arrays.asList(base));
      merged.addAll(additional);

      return merged.toArray(URL[]::new);
    }
  }
}
