package com.webforj.router;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * The {@code RouteCompiler} class is responsible for compiling route templates into regular
 * expressions.
 *
 * <p>
 * The class provides methods to parse a route template string and convert it into a regular
 * expression.
 * </p>
 *
 * @see RoutePattern
 *
 * @since 24.11
 */
public class RouteCompiler {

  /**
   * Compiles a route template into a {@code RoutePattern} object.
   *
   * <p>
   * The method parses the route template string, converts it into a regular expression, and
   * extracts parameter names. Parameters can be defined with optional modifiers and custom regular
   * expressions.
   * </p>
   *
   * @param route the route template string
   * @return a {@code RoutePattern} object containing the compiled regular expression, parameter
   *         names, and original route template
   */
  public RoutePattern compile(String route) {
    String[] parts = route.split("/");
    StringBuilder regexBuilder = new StringBuilder();
    List<String> parameterNames = new ArrayList<>();

    for (String part : parts) {
      if (part.startsWith(":")) {
        String paramName = part.substring(1);
        String regexPart = "([^/]+)";

        if (paramName.endsWith("?")) {
          paramName = paramName.substring(0, paramName.length() - 1);
          regexPart = "([^/]*)";
        } else if (paramName.endsWith("*")) {
          paramName = paramName.substring(0, paramName.length() - 1);
          regexPart = "(.*)";
        } else if (paramName.contains("(") && paramName.contains(")")) {
          int regexStart = paramName.indexOf('(');
          int regexEnd = paramName.indexOf(')');
          regexPart = paramName.substring(regexStart + 1, regexEnd);
          paramName = paramName.substring(0, regexStart);
        }

        regexBuilder.append("/").append(regexPart);
        parameterNames.add(paramName);
      } else {
        regexBuilder.append("/").append(Pattern.quote(part));
      }
    }

    return new RoutePattern(Pattern.compile(regexBuilder.toString()), parameterNames, route);
  }
}
