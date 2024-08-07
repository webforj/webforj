package com.webforj.router;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents a route pattern for URL matching and URL generation.
 *
 * <p>
 * This class enables defining route patterns that include named parameters, optional parameters,
 * and wildcard segments. It allows matching paths against these patterns and generating URLs from
 * parameter maps.
 * </p>
 *
 * <p>
 * Route patterns use a syntax to specify route segments and their constraints:
 * <ul>
 * <li>Named Parameters: Parameters prefixed with a colon (e.g., ":param"). These are required
 * unless marked as optional.</li>
 * <li>Optional Parameters: Named parameters followed by a question mark (e.g., ":param?"). These
 * parameters are optional.</li>
 * <li>Wildcard Segments: A segment denoted by an asterisk (e.g., "*") captures any remaining path
 * segments. It can be named for specificity (e.g., ":path*").</li>
 * <li>Regular Expression Constraints: Parameters can have regular expression constraints (e.g.,
 * ":param<[0-9]+>").</li>
 * </ul>
 * </p>
 *
 * <p>
 * Examples:
 * <ul>
 * <li>Pattern: "/customer/:id<[0-9]+>/named/:name/*"
 * <ul>
 * <li>Matches: "/customer/123/named/john/doe"</li>
 * <li>Extracts: id="123", name="john", "*=doe"</li>
 * </ul>
 * </li>
 * <li>Pattern: "/product/:identifier/:category?/resource/:id<[0-9]*>/:path*"
 * <ul>
 * <li>Matches: "/product/abc/resource/456/docs"</li>
 * <li>Extracts: identifier="abc", category=null, id="456", path="docs"</li>
 * </ul>
 * </li>
 * </ul>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.11
 */
public class RoutePattern {
  private final String pattern;
  private final Pattern regexPattern;
  private final List<String> paramNames = new ArrayList<>();
  private boolean hasWildcard = false;

  /**
   * Constructs a RoutePattern from a route pattern string.
   *
   * <p>
   * The pattern string can include parameters and constraints:
   * <ul>
   * <li><b>Named Parameters:</b> Indicated by ":paramName". These parameters must be provided when
   * constructing URLs or matching paths.</li>
   * <li><b>Optional Parameters:</b> Indicated by ":paramName?". These parameters are not required
   * and can be omitted.</li>
   * <li><b>Wildcard Segments:</b> Indicated by "*". Captures all remaining path segments after the
   * defined pattern.</li>
   * <li><b>Regular Expression Constraints:</b> Added within angle brackets (e.g.,
   * ":paramName<[0-9]+>"). These constraints validate the parameter values.</li>
   * </ul>
   * </p>
   *
   * @param pattern the route pattern string (e.g., "/customer/:id<[0-9]+>/named/:name/*")
   */
  public RoutePattern(String pattern) {
    this.pattern = pattern;
    String regex = buildRegexFromPattern(pattern);
    this.regexPattern = Pattern.compile(regex);
  }

  /**
   * Checks if the provided path matches the route pattern.
   *
   * <p>
   * The path is tested against the compiled regular expression derived from the route pattern. The
   * method returns true if the path adheres to the pattern, otherwise false.
   * </p>
   *
   * @param path the path to check
   * @return true if the path matches the pattern, false otherwise
   */
  public boolean matches(String path) {
    // if path does not start with a slash, add it
    if (!path.startsWith("/")) {
      path = "/" + path; // NOSONAR
    }

    return regexPattern.matcher(path).matches();
  }

  /**
   * Extracts the parameters from the path if it matches the route pattern.
   *
   * <p>
   * The method extracts parameter values from the path based on the route pattern. Parameters are
   * initialized with {@code null} values and updated with actual values from the path if they are
   * present. Wildcard parameters capture any remaining path segments and are handled separately.
   * </p>
   *
   * @param path the path to extract parameters from
   * @return a map of parameter names to values if matched, otherwise an empty map
   */
  public Map<String, String> extractParameters(String path) {
    if (!path.startsWith("/")) {
      path = "/" + path; // NOSONAR
    }

    Matcher matcher = regexPattern.matcher(path);
    if (!matcher.matches()) {
      return Collections.emptyMap();
    }

    Map<String, String> params = new HashMap<>();
    // Initialize all parameters with null values
    for (String paramName : paramNames) {
      params.put(paramName, null);
    }

    int groupIndex = 1; // Start from 1 because group 0 is the entire match
    for (int i = 0; i < paramNames.size(); i++) { // NOSONAR
      String paramName = paramNames.get(i);
      if (groupIndex > matcher.groupCount()) {
        break; // No more groups to process
      }

      String value = matcher.group(groupIndex);
      if ("*".equals(paramName)) {
        params.put(paramName, value != null ? value : "");
        break; // Wildcard captures remaining segments, stop further processing
      }

      if (value != null) {
        params.put(paramName, value);
      }

      groupIndex++;
    }

    return Collections.unmodifiableMap(params);
  }

  /**
   * Gets the original route pattern string.
   *
   * <p>
   * This method returns the pattern string as provided during the construction of the
   * {@code RoutePattern} instance.
   * </p>
   *
   * @return the original pattern string
   */
  public String getPattern() {
    return pattern;
  }

  /**
   * Gets the compiled regular expression pattern.
   *
   * <p>
   * This method returns the regular expression pattern used for matching paths against the route
   * pattern.
   * </p>
   *
   * @return the regular expression pattern
   */
  public Pattern getRouteRegex() {
    return regexPattern;
  }

  /**
   * Gets the list of parameter names in the order they appear in the pattern.
   *
   * <p>
   * The list includes all named parameters and wildcard segments extracted from the route pattern.
   * This list is used to map extracted values from the path and to construct URLs.
   * </p>
   *
   * @return the list of parameter names
   */
  public List<String> getParameterNames() {
    return paramNames;
  }

  /**
   * Checks if the pattern contains a wildcard segment.
   *
   * <p>
   * Wildcards are used to capture any remaining path segments and are indicated by "*" in the
   * pattern. This method checks if such a segment is present in the pattern.
   * </p>
   *
   * @return true if the pattern has a wildcard, false otherwise
   */
  public boolean hasWildcard() {
    return hasWildcard;
  }

  /**
   * Constructs a URL from a RoutePattern and a map of parameters.
   *
   * <p>
   * The URL is constructed by replacing parameters in the pattern with corresponding values from
   * the provided map. Wildcard segments capture remaining path components.
   * </p>
   *
   * <p>
   * Rules for URL construction:
   * <ul>
   * <li>If the pattern includes named parameters (e.g., ":paramName"), the corresponding values
   * from the map replace these parameters.</li>
   * <li>If the pattern includes optional parameters (e.g., ":paramName?"), these are appended to
   * the URL only if present in the map.</li>
   * <li>If the pattern includes wildcard segments (e.g., "*", ":paramName*"), the map should
   * provide a value for "*" or ":paramName*" that captures the remaining path segments.</li>
   * <li>Static segments in the pattern are directly appended to the URL.</li>
   * <li>The method trims trailing slashes from the URL unless the original pattern ends with a
   * slash.</li>
   * </ul>
   * </p>
   *
   * @param params a map of parameter names to values
   * @return the constructed URL
   * @throws IllegalArgumentException if a required parameter is missing in the provided map
   */
  public String buildUrl(Map<String, String> params) { // NOSONAR
    StringBuilder urlBuilder = new StringBuilder();
    String[] parts = getPattern().split("/");

    boolean endsWithSlash = getPattern().endsWith("/");

    for (int i = 0; i < parts.length; i++) { // NOSONAR
      String part = parts[i];

      if (part.isEmpty()) {
        continue;
      }

      if (part.startsWith(":")) {
        // Extract parameter name
        String paramName = part.substring(1).replaceAll("[?<].*", "");
        boolean isNamedWildcard = part.endsWith("*");

        if (isNamedWildcard) {
          // Handle named wildcard
          String wildcardValue = params.get(paramName);
          if (wildcardValue != null) {
            urlBuilder.append("/").append(wildcardValue);
          }

          // Skip further processing for named wildcard
          continue;
        }

        // Handle optional parameters
        if (part.contains("?")) {
          String value = params.get(paramName);
          if (value != null) {
            urlBuilder.append("/").append(value);
          }
          continue;
        }

        // Required parameter
        String value = params.get(paramName);
        if (value == null) {
          throw new IllegalArgumentException("Missing required parameter: " + paramName);
        }

        urlBuilder.append("/").append(value);
      } else if (part.equals("*")) {
        // Handle plain wildcard segment
        String wildcardValue = params.get("*");
        if (wildcardValue != null) {
          urlBuilder.append("/").append(wildcardValue);
        }
      } else {
        // Append static segments
        urlBuilder.append("/").append(part);
      }
    }

    // Ensure trailing slash handling
    if (!endsWithSlash && urlBuilder.length() > 0
        && urlBuilder.charAt(urlBuilder.length() - 1) == '/') {
      urlBuilder.deleteCharAt(urlBuilder.length() - 1);
    }

    return urlBuilder.toString();
  }

  /**
   * Converts the route pattern into a regular expression and identifies parameter names.
   *
   * @param pattern the route pattern string
   * @return the regular expression corresponding to the pattern
   */
  private String buildRegexFromPattern(String pattern) { // NOSONAR
    StringBuilder regexBuilder = new StringBuilder("^");
    String[] parts = pattern.split("/");

    for (String part : parts) { // NOSONAR
      if (part.isEmpty()) {
        continue;
      }

      if (part.equals("*")) {
        hasWildcard = true;
        regexBuilder.append("(?:/(.*))?");
        paramNames.add("*");
        continue;
      }

      @SuppressWarnings("squid:S6035")
      Matcher matcher = Pattern.compile(":([a-zA-Z_][\\w]*)(\\*|\\?)?(?:<(.*?)>)?").matcher(part);
      if (matcher.matches()) {
        String paramName = matcher.group(1);
        String modifier = matcher.group(2);
        String regex = matcher.group(3);

        if ("?".equals(modifier)) {
          regexBuilder.append("(?:");
          regexBuilder.append("/");
          if (regex == null) {
            regexBuilder.append("([^/]+)");
          } else {
            regexBuilder.append("(").append(regex).append(")");
          }
          regexBuilder.append(")?");
          paramNames.add(paramName);
        } else if ("*".equals(modifier)) {
          regexBuilder.append("(?:/(.*))?");
          paramNames.add(paramName); // Wildcard, to capture remaining segments
        } else {
          regexBuilder.append("/");
          if (regex == null) {
            regexBuilder.append("([^/]+)");
          } else {
            regexBuilder.append("(").append(regex).append(")");
          }
          paramNames.add(paramName);
        }
      } else {
        regexBuilder.append("/").append(Pattern.quote(part));
      }
    }

    if (!hasWildcard) {
      regexBuilder.append("/?");
    }

    regexBuilder.append("$");
    return regexBuilder.toString();
  }
}
