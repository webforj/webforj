package com.webforj.router;

import com.webforj.router.history.ParametersBag;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Represents a route pattern for URL matching and URL generation.
 *
 * <p>
 * This class enables defining route patterns that include named parameters, optional parameters,
 * wildcard segments, and layout components. It allows matching paths against these patterns and
 * generating URLs from parameter maps.
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
 * <li>Layout Components: Segments starting with "@" (e.g., "/@layout") are treated as layout
 * components. These segments are ignored for the purpose of matching and URL generation, and are
 * meant to define layout structures within applications.</li>
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
 * <li>Pattern: "/@layout/product/:identifier/:category?/resource/:id<[0-9]*>/:path*"
 * <ul>
 * <li>Matches: "/product/abc/resource/456/docs"</li>
 * <li>Extracts: identifier="abc", category=null, id="456", path="docs"</li>
 * </ul>
 * <li>Layout segment "@layout" is ignored during matching.</li>
 * </ul>
 * </li>
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
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
   * The pattern string can include parameters, constraints, and layout components:
   * <ul>
   * <li><b>Named Parameters:</b> Indicated by ":paramName". These parameters must be provided when
   * constructing URLs or matching paths.</li>
   * <li><b>Optional Parameters:</b> Indicated by ":paramName?". These parameters are not required
   * and can be omitted.</li>
   * <li><b>Wildcard Segments:</b> Indicated by "*". Captures all remaining path segments after the
   * defined pattern.</li>
   * <li><b>Regular Expression Constraints:</b> Added within angle brackets (e.g.,
   * ":paramName<[0-9]+>"). These constraints validate the parameter values.</li>
   * <li><b>Layout Components:</b> Indicated by segments starting with "@" (e.g., "/@layout"). These
   * are ignored during URL matching and generation, and are meant to specify layouts within the
   * application's routing structure.</li>
   * </ul>
   * </p>
   *
   * @param pattern the route pattern string (e.g., "/customer/:id<[0-9]+>/named/:name/*")
   * @throws IllegalArgumentException if the route pattern contains only layout components and no
   *         actual route segments.
   */
  public RoutePattern(String pattern) {
    Objects.requireNonNull(pattern, "Pattern cannot be null");
    this.pattern = pattern;
    String regex = buildRegexFromPattern(RouterUtils.normalizePath(pattern));
    this.regexPattern = Pattern.compile(regex);
  }

  /**
   * Checks if the provided path matches the route pattern.
   *
   * <p>
   * The path is tested against the compiled regular expression derived from the route pattern. The
   * method returns true if the path adheres to the pattern, otherwise false. Layout components
   * (segments prefixed with "@") are ignored during matching.
   * </p>
   *
   * @param path the path to check
   * @return true if the path matches the pattern, false otherwise
   */
  public boolean matches(String path) {
    Objects.requireNonNull(path, "Path cannot be null");
    return regexPattern.matcher(RouterUtils.normalizePath(path)).matches();
  }

  /**
   * Extracts the parameters from the path if it matches the route pattern.
   *
   * <p>
   * The method extracts parameter values from the path based on the route pattern. Parameters are
   * initialized with {@code null} values and updated with actual values from the path if they are
   * present. Wildcard parameters capture any remaining path segments and are handled separately.
   * Layout components (segments prefixed with "@") are ignored during parameter extraction.
   * </p>
   *
   * @param path the path to extract parameters from
   * @return a map of parameter names to values if matched, otherwise an empty map
   */
  public Map<String, String> getParameters(String path) {
    path = RouterUtils.normalizePath(path);

    Matcher matcher = regexPattern.matcher(path);
    if (!matcher.matches()) {
      return Collections.emptyMap();
    }

    Map<String, String> params = new HashMap<>();
    for (String paramName : paramNames) {
      params.put(paramName, null);
    }

    int groupIndex = 1;
    for (int i = 0; i < paramNames.size(); i++) { // NOSONAR
      String paramName = paramNames.get(i);
      if (groupIndex > matcher.groupCount()) {
        break;
      }

      String value = matcher.group(groupIndex);
      if ("*".equals(paramName)) {
        params.put(paramName, value != null ? value : "");
        break;
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
   * {@code RoutePattern} instance. Layout components are included in the returned pattern string.
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
  public Pattern getRegex() {
    return regexPattern;
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
   * the provided map. Wildcard segments capture remaining path components. Layout components
   * (segments prefixed with "@") are ignored during URL generation.
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
  public String generateUrl(Map<String, String> params) { // NOSONAR
    StringBuilder urlBuilder = new StringBuilder();
    String[] parts = getPattern().split("/");

    boolean endsWithSlash = getPattern().endsWith("/");

    for (int i = 0; i < parts.length; i++) { // NOSONAR
      String part = parts[i];

      if (part.isEmpty() || part.startsWith("@")) {
        continue;
      }

      if (part.startsWith(":")) {
        String paramName = part.substring(1).replaceAll("[?<].*", "");
        boolean isNamedWildcard = part.endsWith("*");

        if (isNamedWildcard) {
          String wildcardValue = params.get(paramName);
          if (wildcardValue != null) {
            urlBuilder.append("/").append(wildcardValue);
          }
          continue;
        }

        if (part.contains("?")) {
          String value = params.get(paramName);
          if (value != null) {
            urlBuilder.append("/").append(value);
          }
          continue;
        }

        String value = params.get(paramName);
        if (value == null) {
          throw new IllegalArgumentException("Missing required parameter: " + paramName);
        }

        urlBuilder.append("/").append(value);
      } else if (part.equals("*")) {
        String wildcardValue = params.get("*");
        if (wildcardValue != null) {
          urlBuilder.append("/").append(wildcardValue);
        }
      } else {
        urlBuilder.append("/").append(part);
      }
    }

    if (!endsWithSlash && urlBuilder.length() > 0
        && urlBuilder.charAt(urlBuilder.length() - 1) == '/') {
      urlBuilder.deleteCharAt(urlBuilder.length() - 1);
    }

    return urlBuilder.toString();
  }

  /**
   * Constructs a URL from a RoutePattern and a {@code ParametersBag}.
   *
   * <p>
   * This method behaves similarly to {@link #generateUrl(Map)}, but uses a {@code ParametersBag} to
   * provide the parameters for URL construction. Layout components (segments prefixed with "@") are
   * ignored during URL generation.
   * </p>
   *
   * @param params a {@code ParametersBag} containing parameter names and values
   * @return the constructed URL
   * @throws IllegalArgumentException if a required parameter is missing in the provided map
   */
  public String generateUrl(ParametersBag params) { // NOSONAR
    return generateUrl(params.all());
  }

  /**
   * Converts the route pattern into a regular expression and identifies parameter names.
   *
   * <p>
   * This method parses the provided route pattern and constructs a regular expression used for
   * matching paths. It identifies named, optional, and wildcard parameters, as well as any regular
   * expression constraints. Layout components (segments prefixed with "@") are ignored when
   * building the regular expression.
   * </p>
   *
   * @param pattern the route pattern string
   * @return the regular expression corresponding to the pattern
   */
  private String buildRegexFromPattern(String pattern) { // NOSONAR
    StringBuilder regexBuilder = new StringBuilder("^");
    String[] parts = pattern.split("/");
    parts = Arrays.stream(parts).filter(part -> !part.startsWith("@")).toArray(String[]::new);
    if (parts.length == 0 && !pattern.equals("/")) {
      return "^$";
    }

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
          paramNames.add(paramName);
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
