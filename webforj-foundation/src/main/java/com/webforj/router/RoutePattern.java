package com.webforj.router;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RoutePattern {
  private final String template;
  private final Pattern pattern;
  private final String[] parameterNames;

  public RoutePattern(String template) {
    this.template = template;
    this.parameterNames = extractParameterNames(template);
    this.pattern = Pattern.compile(buildRegexFromTemplate(template));
  }

  private String buildRegexFromTemplate(String template) {
    StringBuilder regex = new StringBuilder("^");
    Matcher matcher = Pattern.compile("(:\\w+)(\\(([^)]+)\\))?([*?]?)").matcher(template);
    int lastPos = 0;

    while (matcher.find()) {
      regex.append(Pattern.quote(template.substring(lastPos, matcher.start())));
      String paramName = matcher.group(1).substring(1); // Remove colon
      String customRegex = matcher.group(3);
      String modifier = matcher.group(4);

      if (customRegex != null) {
        regex.append("(?<").append(paramName).append(">").append(customRegex).append(")");
      } else if ("*".equals(modifier)) {
        regex.append("(?<").append(paramName).append(">.+)");
      } else if ("?".equals(modifier)) {
        regex.append("(?<").append(paramName).append(">[^/]*)?");
      } else {
        regex.append("(?<").append(paramName).append(">[^/]+)");
      }

      lastPos = matcher.end();
    }
    regex.append(Pattern.quote(template.substring(lastPos)));
    regex.append("$");

    return regex.toString();
  }

  private String[] extractParameterNames(String template) {
    Matcher matcher = Pattern.compile(":\\w+").matcher(template);
    return matcher.results().map(m -> m.group().substring(1)).toArray(String[]::new);
  }

  public String composePath(Map<String, String> parameters) {
    String path = template;
    for (Map.Entry<String, String> entry : parameters.entrySet()) {
      path = path.replace(":" + entry.getKey(), entry.getValue());
    }
    path = path.replaceAll("/:[^/]+\\*", "");
    path = path.replaceAll("/:[^/]+\\?", "");
    return path;
  }

  public boolean matches(String url) {
    Matcher matcher = pattern.matcher(url);
    return matcher.matches();
  }

  public Map<String, String> extractParameters(String url) {
    Matcher matcher = pattern.matcher(url);
    Map<String, String> parameters = new HashMap<>();
    if (matcher.matches()) {
      for (String name : parameterNames) {
        parameters.put(name, matcher.group(name));
      }
    }
    return parameters;
  }

  public String getTemplate() {
    return template;
  }

  public Pattern getPattern() {
    return pattern;
  }
}

