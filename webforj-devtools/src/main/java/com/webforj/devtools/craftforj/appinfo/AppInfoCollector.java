package com.webforj.devtools.craftforj.appinfo;

import com.webforj.App;
import com.webforj.Environment;
import com.webforj.devtools.craftforj.appinfo.model.AppInfo;
import com.webforj.devtools.craftforj.capabilities.VersionDetector;
import java.io.IOException;
import java.io.InputStream;
import java.lang.management.ManagementFactory;
import java.nio.file.Path;
import java.util.Properties;

/**
 * Collects environment information about the running app.
 *
 * <p>
 * The framework version is resolved once at construction time. Application state (name, context
 * path) is read live on every {@link #collect()} call so the frontend always sees current values.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class AppInfoCollector {

  private static final String WEBFORJ_POM_PROPERTIES =
      "META-INF/maven/com.webforj/webforj-foundation/pom.properties";

  private final String appClass;
  private final String projectRoot;
  private final String webforjVersion;

  /**
   * Creates a new collector.
   *
   * @param appClass the fully qualified app class name
   * @param projectRoot the project root directory
   */
  public AppInfoCollector(String appClass, Path projectRoot) {
    this.appClass = appClass;
    this.projectRoot = projectRoot == null ? null : projectRoot.toString();
    this.webforjVersion = detectWebforjVersion();
  }

  /**
   * Collects the current application information.
   *
   * @return the collected information
   */
  public AppInfo collect() {
    AppInfo info = new AppInfo();
    info.setAppName(App.getApplicationName());
    info.setAppClass(appClass);
    info.setContextPath(Environment.getContextPath());
    info.setProjectRoot(projectRoot);
    info.setWebforjVersion(webforjVersion);
    info.setBbjServices(Environment.isRunningWithBBjServices());
    info.setJavaVersion(System.getProperty("java.version"));
    info.setJavaVendor(System.getProperty("java.vendor"));
    info.setJavaVm(System.getProperty("java.vm.name"));
    info.setOsName(System.getProperty("os.name"));
    info.setOsVersion(System.getProperty("os.version"));
    info.setOsArch(System.getProperty("os.arch"));
    info.setStartedAt(ManagementFactory.getRuntimeMXBean().getStartTime());

    return info;
  }

  /**
   * Detects the webforJ framework version.
   *
   * <p>
   * The foundation Maven descriptor names the webforJ actually on the classpath and is preferred.
   * Maven writes it while packaging the jar, so a build that puts webforJ on the classpath as
   * compiled classes carries none, and the version craftforJ itself was built at names the same
   * release.
   * </p>
   *
   * @return the version string, or {@code null} if neither descriptor is present
   */
  private static String detectWebforjVersion() {
    String version = readFoundationVersion();
    return version != null ? version : VersionDetector.moduleVersion();
  }

  /**
   * Reads the version from the foundation Maven descriptor.
   *
   * @return the version string, or {@code null} if the descriptor is absent or unreadable
   */
  private static String readFoundationVersion() {
    try (InputStream is =
        Environment.class.getClassLoader().getResourceAsStream(WEBFORJ_POM_PROPERTIES)) {
      if (is == null) {
        return null;
      }

      Properties props = new Properties();
      props.load(is);

      return props.getProperty("version");
    } catch (IOException e) {

      return null;
    }
  }
}
