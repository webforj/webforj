package com.webforj;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.startup.type.BBjException;
import com.typesafe.config.Config;
import com.webforj.annotation.AppEntry;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.conceiver.ConceiverProvider;
import com.webforj.environment.StringTable;
import com.webforj.exceptions.WebforjAppInitializeException;
import com.webforj.exceptions.WebforjException;
import io.github.classgraph.ClassGraph;
import io.github.classgraph.ClassInfo;
import io.github.classgraph.ScanResult;
import java.util.List;
import java.util.Map;

/**
 * The {@code Bootstrap} class is responsible for initializing and launching the main application in
 * webforJ.
 *
 * <p>
 * {@code Bootstrap} performs classpath scanning to find subclasses of {@link App} in the classpath
 * and attempts to locate the entry point of the application. If a class is annotated with
 * {@link AppEntry}, that class is selected as the entry point. If multiple classes are annotated
 * with {@link AppEntry}, an error is thrown. If no annotation is found, the first discovered
 * subclass of {@code App} is used only if it is the sole subclass present. Otherwise, detailed
 * error messages are provided, including a list of discovered classes and whether or not they are
 * annotated.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
public final class Bootstrap {

  private Bootstrap() {}

  /**
   * Initializes the environment and launches the main application.
   *
   * <p>
   * This method initializes the environment and launches the main application. The main application
   * is determined by scanning the classpath for subclasses of {@link App} and selecting the
   * appropriate class. If multiple subclasses of {@code App} are found, the following rules apply:
   *
   * <ul>
   * <li>If a class annotated with {@link AppEntry} is found, that class is selected.</li>
   * <li>If multiple classes are annotated with {@link AppEntry}, an error is thrown, providing
   * details of all discovered classes.</li>
   * <li>If no annotation is found, and there is only one subclass of {@code App}, that class is
   * selected.</li>
   * <li>If no annotation is found and there are multiple subclasses of {@code App}, an error is
   * thrown with detailed information about each subclass.</li>
   * </ul>
   * It is possible to bypass classpath scanning by providing the class name directly.
   * </p>
   *
   * @param api the BBjAPI instance.
   * @param bridge the WebforjBBjBridge instance.
   * @param debug {@code 1} if debug mode is enabled, {@code 0} otherwise.
   * @param className the fully qualified class name of the application to launch, or {@code null}
   *        to automatically detect it.
   *
   * @throws BBjException if there is an error initializing the environment.
   * @throws WebforjException if there is an error initializing the application, such as if no valid
   *         application class is found or if multiple annotated application classes are detected.
   */
  public static void init(BBjAPI api, WebforjBBjBridge bridge, int debug, String className)
      throws BBjException, WebforjException {
    Environment.init(api, bridge, debug);
    processConfig();
    initApplication(className);
  }

  /**
   * Launches the main application.
   *
   * @param className the fully qualified class name of the application to launch, or {@code null}
   */
  private static void initApplication(String className) throws WebforjException {
    String selectedClassName = detectClassName(className);

    if (selectedClassName == null || selectedClassName.isEmpty()) {
      throw new WebforjAppInitializeException("Failed to determine application entry point."
          + " No className provided and no application class found.");
    }

    try {
      @SuppressWarnings("unchecked")
      App app = ConceiverProvider.getCurrent()
          .getApplication((Class<? extends App>) Class.forName(selectedClassName));
      app.initialize();
    } catch (ClassNotFoundException e) {
      throw new WebforjAppInitializeException("Failed to find application class '"
          + selectedClassName + "'." + " Ensure the class is in the classpath.", e);
    }
  }

  /**
   * Determines the class name of the application to launch.
   *
   * @param className the class name provided, or {@code null} to automatically detect it.
   */
  private static String detectClassName(String className) throws WebforjAppInitializeException {
    // if a class name is provided, use it
    if (className != null && !className.isEmpty()) {
      return className;
    }

    // check if the entry point is provided in the configuration
    Config config = Environment.getCurrent().getConfig();
    String entryProp = "webforj.entry";
    if (config.hasPath(entryProp) && !config.getIsNull(entryProp)) {
      String entry = config.getString(entryProp);
      if (entry != null && !entry.isEmpty()) {
        return entry;
      }
    }

    // entry cannot be determined, scan the classpath
    return scanClassPathForEntry();
  }

  /**
   * Scans the classpath to find the appropriate application class.
   *
   * <p>
   * This method scans for all subclasses of {@link App} and checks for the presence of the
   * {@link AppEntry} annotation. Detailed error information is provided if multiple subclasses of
   * {@code App} are found, or if none are annotated with {@link AppEntry}.
   * </p>
   *
   * <ul>
   * <li>If a class annotated with {@code @AppEntry} is found, that class is selected.</li>
   * <li>If multiple classes are annotated with {@code @AppEntry}, an exception is thrown with
   * details on each discovered class.</li>
   * <li>If no annotated class is found, and only one subclass of {@code App} exists, that class is
   * selected.</li>
   * <li>If no annotated class is found, and multiple subclasses of {@code App} exist, an exception
   * is thrown with details about each subclass.</li>
   * </ul>
   *
   * @return the name of the class that is determined to be the entry point of the application.
   * @throws WebforjAppInitializeException if there is an error during classpath scanning, such as
   *         if no subclasses of {@code App} are found, multiple subclasses are found without
   *         annotations, or multiple {@code @AppEntry} annotations are detected.
   */
  private static String scanClassPathForEntry() throws WebforjAppInitializeException {
    String selectedClassName = null;
    try (ScanResult scanResult = new ClassGraph()
        .filterClasspathElements(classpathElement -> !classpathElement
            .startsWith(System.getProperty("basis.BBjHome") + "/.lib"))
        .enableAnnotationInfo().enableClassInfo().scan()) {

      List<ClassInfo> subclasses = scanResult.getSubclasses("com.webforj.App");

      // no application class found
      if (subclasses.isEmpty()) {
        throw new WebforjAppInitializeException("No application class found."
            + " Ensure you have a subclass of com.webforj.App in your project.");
      }

      List<ClassInfo> annotatedClasses =
          scanResult.getClassesWithAnnotation("com.webforj.annotation.AppEntry");

      // more than one application class found
      if (subclasses.size() > 1) {
        StringBuilder errorInfo =
            new StringBuilder("Multiple subclasses of com.webforj.App found:\n");
        for (ClassInfo subclass : subclasses) {
          boolean isAnnotated = annotatedClasses.contains(subclass);
          errorInfo.append("  - ").append(subclass.getName())
              .append(isAnnotated ? " (annotated with @AppEntry)" : "").append("\n");
        }

        // multiple subclasses of App found, and multiple are annotated with @AppEntry
        if (annotatedClasses.size() > 1) {
          errorInfo
              .append("Error: Multiple classes are annotated with @AppEntry. Only one is allowed.");
          throw new WebforjAppInitializeException(errorInfo.toString());
        } else if (annotatedClasses.isEmpty()) {
          errorInfo.append(
              "Error: No subclass is annotated with @AppEntry. At least one must be annotated.");
          throw new WebforjAppInitializeException(errorInfo.toString());
        } else {
          // only one application class found and annotated with @AppEntry
          selectedClassName = annotatedClasses.get(0).getName();
        }
      } else {
        // only one application class found
        selectedClassName = subclasses.get(0).getName();
      }
    }

    return selectedClassName;
  }

  /**
   * Processes the configuration file.
   */
  private static void processConfig() {
    Config config = Environment.getCurrent().getConfig();

    // Set debug mode
    String debugProp = "webforj.debug";
    Boolean isDebug =
        config.hasPath(debugProp) && !config.getIsNull(debugProp) ? config.getBoolean(debugProp)
            : null;
    if (isDebug != null) {
      Environment.getCurrent().setDebug(isDebug);
    }

    // update the string table
    String stringTableProp = "webforj.stringTable";
    if (config.hasPath(stringTableProp) && !config.getIsNull(stringTableProp)) {
      Map<String, Object> stringTable = config.getObject(stringTableProp).unwrapped();
      for (Map.Entry<String, Object> entry : stringTable.entrySet()) {
        StringTable.put(entry.getKey(), String.valueOf(entry.getValue()));
      }
    }

    // Set the components base
    String componentsProp = "webforj.components";
    String components = config.hasPath(componentsProp) && !config.getIsNull(componentsProp)
        ? config.getString(componentsProp)
        : null;
    if (components != null && !components.isEmpty()) {
      StringTable.put("!COMPONENTS", components);
    }

    // Set the locale
    String localeProp = "webforj.locale";
    String locale =
        config.hasPath(localeProp) && !config.getIsNull(localeProp) ? config.getString(localeProp)
            : null;

    if (locale != null && !locale.isEmpty()) {
      StringTable.put("!LOCALE", locale);
    }
  }
}
