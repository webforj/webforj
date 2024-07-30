package com.webforj.router;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;

/**
 * The {@code RouteMatcher} class is responsible for matching paths against compiled route patterns
 * and extracting parameters from the matched paths.
 *
 * <p>
 * This class provides methods to check if a path matches a route pattern and to extract parameters
 * from the matched path.
 * </p>
 *
 * @see RoutePattern
 *
 * @since 24.11
 */
public class RouteMatcher {

  /**
   * Matches a given path against a {@code RoutePattern} and checks if it matches.
   *
   * @param path the path to match
   * @param routePattern the {@code RoutePattern} object containing the regular expression and
   *        parameter names
   * @return {@code true} if the path matches the route pattern, {@code false} otherwise
   */
  public boolean matches(String path, RoutePattern routePattern) {
    Matcher matcher = routePattern.getRegex().matcher(path);
    return matcher.matches();
  }

  /**
   * Extracts parameter values from a matched path against a {@code RoutePattern}.
   *
   * @param path the path to extract parameters from
   * @param routePattern the {@code RoutePattern} object containing the regular expression and
   *        parameter names
   * @return a {@code Map} of parameter names and their values if the path matches, an empty
   *         {@code Map} otherwise
   */
  public Map<String, String> extractParameters(String path, RoutePattern routePattern) {
    Matcher matcher = routePattern.getRegex().matcher(path);
    Map<String, String> parameters = new HashMap<>();

    if (matcher.matches()) {
      List<String> parameterNames = routePattern.getParameterNames();
      for (int i = 0; i < parameterNames.size(); i++) {
        if (parameterNames.get(i) != null) {
          parameters.put(parameterNames.get(i), matcher.group(i + 1));
        }
      }
    }

    return parameters;
  }
}
