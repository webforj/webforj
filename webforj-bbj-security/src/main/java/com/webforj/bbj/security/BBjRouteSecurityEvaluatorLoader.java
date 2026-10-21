package com.webforj.bbj.security;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.ServiceConfigurationError;
import java.util.ServiceLoader;

/**
 * Finds the custom route security evaluators of an app.
 *
 * <p>
 * Evaluators are {@link BBjRouteSecurityEvaluator} implementations listed in
 * {@code META-INF/services/com.webforj.bbj.security.BBjRouteSecurityEvaluator} and need a public
 * constructor without parameters.
 * </p>
 *
 * @author Christian Liebhardt
 * @since 26.03
 */
final class BBjRouteSecurityEvaluatorLoader {
  private static final Logger logger =
      System.getLogger(BBjRouteSecurityEvaluatorLoader.class.getName());

  private BBjRouteSecurityEvaluatorLoader() {}

  /**
   * Loads the listed evaluators.
   *
   * <p>
   * Evaluators of equal priority keep the order of the service file.
   * </p>
   *
   * @param classLoader the class loader that sees the service files of the app, or {@code null} to
   *        use the class loader of this library, which sees them when the library sits on the
   *        classpath of the app
   * @return the evaluators, empty when none is listed
   *
   * @throws ServiceConfigurationError when a listed evaluator cannot be loaded or created
   */
  static List<BBjRouteSecurityEvaluator> loadEvaluators(ClassLoader classLoader) {
    List<BBjRouteSecurityEvaluator> evaluators = new ArrayList<>();
    ClassLoader serviceClassLoader =
        classLoader != null ? classLoader : BBjRouteSecurityEvaluatorLoader.class.getClassLoader();

    try {
      for (BBjRouteSecurityEvaluator evaluator : ServiceLoader.load(BBjRouteSecurityEvaluator.class,
          serviceClassLoader)) {
        evaluators.add(evaluator);
      }
    } catch (ServiceConfigurationError error) {
      logger.log(Level.ERROR,
          "Route security evaluator could not be loaded, the app does not start. {0}",
          error.getMessage());
      throw error;
    }

    return evaluators;
  }
}
