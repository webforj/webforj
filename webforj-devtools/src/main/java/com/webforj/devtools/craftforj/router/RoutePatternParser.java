package com.webforj.devtools.craftforj.router;

import com.webforj.devtools.craftforj.router.model.RouteParam;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses route patterns to extract parameter metadata.
 *
 * <p>
 * Extracts parameter names, constraints, optional flags, and wildcard flags from route patterns.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class RoutePatternParser {

  private static final Pattern PARAM_PATTERN =
      Pattern.compile(":([a-zA-Z_][\\w]*)([*?])?(?:<(.*?)>)?");

  private RoutePatternParser() {}

  /**
   * Parses a route pattern and extracts all parameters.
   *
   * @param pattern the route pattern (e.g., "/products/:id/:name?/:path*")
   * @return list of RouteParam, or empty list if no parameters
   */
  public static List<RouteParam> parse(String pattern) {
    if (pattern == null || pattern.isEmpty()) {
      return Collections.emptyList();
    }

    List<RouteParam> params = new ArrayList<>();
    String[] parts = pattern.split("/");

    for (String part : parts) {
      if (part.isEmpty() || part.startsWith("@")) {
        continue;
      }

      if (part.equals("*")) {
        params.add(RouteParam.wildcard("*"));
        continue;
      }

      Matcher matcher = PARAM_PATTERN.matcher(part);
      if (matcher.matches()) {
        String name = matcher.group(1);
        String modifier = matcher.group(2);
        String constraint = matcher.group(3);

        boolean optional = "?".equals(modifier);
        boolean wildcard = "*".equals(modifier);

        RouteParam param = new RouteParam();
        param.setName(name);
        param.setConstraint(constraint);
        param.setOptional(optional);
        param.setWildcard(wildcard);
        params.add(param);
      }
    }

    return Collections.unmodifiableList(params);
  }
}
