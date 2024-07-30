package com.webforj.router;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RouteFormat {
  private final Pattern regexPattern;
  private final List<String> paramNames = new ArrayList<>();
  private final String originalPattern;
  private boolean hasWildcard = false;

  public RouteFormat(String pattern) {
    this.originalPattern = pattern;
    String regex = buildRegexFromPattern(pattern);
    this.regexPattern = Pattern.compile(regex);
  }

  private String buildRegexFromPattern(String pattern) {
    StringBuilder regexBuilder = new StringBuilder("^");
    String[] parts = pattern.split("/");

    for (String part : parts) {
      if (part.isEmpty())
        continue;

      if (part.equals("*")) {
        hasWildcard = true;
        regexBuilder.append("(?:/(.*))?");
        paramNames.add("*");
        continue;
      }

      Matcher matcher = Pattern.compile(":([a-zA-Z_][a-zA-Z0-9_]*)(\\?)?(<(.*?)>)?").matcher(part);
      if (matcher.matches()) {
        String paramName = matcher.group(1);
        String optionalFlag = matcher.group(2);
        String regex = matcher.group(4);

        paramNames.add(paramName);

        if (optionalFlag != null) {
          regexBuilder.append("(?:");
        }

        regexBuilder.append("/");

        if (regex == null) {
          regexBuilder.append("([^/]+)");
        } else {
          regexBuilder.append("(").append(regex).append(")");
        }

        if (optionalFlag != null) {
          regexBuilder.append(")?");
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

  public Map<String, String> extractParameters(String path) {
    Matcher matcher = regexPattern.matcher(path);
    if (!matcher.matches()) {
      return null;
    }

    Map<String, String> params = new HashMap<>();
    for (int i = 1; i <= matcher.groupCount(); i++) {
      String paramName = paramNames.get(i - 1);
      String value = matcher.group(i);
      if (paramName.equals("*") && value == null) {
        continue; // No wildcard match found
      }
      params.put(paramName, value);
    }

    return params;
  }

  public boolean matches(String path) {
    return regexPattern.matcher(path).matches();
  }

  public String constructUrl(Map<String, String> params) {
    StringBuilder urlBuilder = new StringBuilder();
    String[] parts = originalPattern.split("/");

    for (String part : parts) {
      if (part.isEmpty())
        continue;

      if (part.startsWith(":")) {
        String paramName = part.substring(1).replaceAll("[?<].*", "");
        String value = params.get(paramName);
        if (value == null && part.contains("?")) {
          continue; // Optional parameter not provided
        } else if (value == null) {
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

    return urlBuilder.toString();
  }

  public static void main(String[] args) {
    RouteFormat rf = new RouteFormat("/customer/:id<[0-9]+>/named/:name/*");

    // Example usage
    String path = "/customer/123/named/john/doe";
    Map<String, String> params = rf.extractParameters(path);

    if (params != null) {
      System.out.println("Matched parameters: " + params);
    } else {
      System.out.println("No match.");
    }

    String url = rf.constructUrl(Map.of("id", "123", "name", "john", "*", "extra/path"));
    System.out.println("Constructed URL: " + url);
  }
}


