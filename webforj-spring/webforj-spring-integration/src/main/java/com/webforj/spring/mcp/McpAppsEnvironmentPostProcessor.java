package com.webforj.spring.mcp;

import com.webforj.spring.WebforjServletConfiguration;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.springframework.boot.EnvironmentPostProcessor;
import org.springframework.boot.SpringApplication;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.MapPropertySource;
import org.springframework.util.ClassUtils;

/**
 * Contributes the environment defaults the MCP apps support needs before the configuration binds.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class McpAppsEnvironmentPostProcessor implements EnvironmentPostProcessor {

  private static final String PROPERTY_SOURCE_NAME = "webforj-mcp-apps-defaults";
  private static final String MCP_APPS_MARKER = "com.webforj.mcp.McpAppContribution";
  private static final String EXCLUDE_URLS = "webforj.exclude-urls";
  private static final String ENDPOINT_PROPERTY = "spring.ai.mcp.server.mcp-endpoint";
  private static final String DEFAULT_ENDPOINT = "/mcp";
  private static final List<String> DISCOVERY_EXCLUSIONS =
      List.of("/.well-known/oauth-protected-resource/**",
          "/.well-known/oauth-authorization-server/**", "/.well-known/openid-configuration/**");
  private static final String ORIGIN = "webforj.origin";
  private static final String COMPONENTS = "webforj.components";
  private static final String SERVLET_MAPPING = "webforj.servlet-mapping";
  private static final String COMPONENTS_SUFFIX = "/webapp/_lib/components";

  @Override
  public void postProcessEnvironment(ConfigurableEnvironment environment,
      SpringApplication application) {
    if (!ClassUtils.isPresent(MCP_APPS_MARKER, getClass().getClassLoader())) {
      return;
    }

    Map<String, Object> defaults = new HashMap<>();
    excludeEndpoint(environment, defaults);
    deriveComponents(environment, defaults);

    if (!defaults.isEmpty()) {
      environment.getPropertySources()
          .addFirst(new MapPropertySource(PROPERTY_SOURCE_NAME, defaults));
    }
  }

  private static void excludeEndpoint(ConfigurableEnvironment environment,
      Map<String, Object> defaults) {
    String endpoint = environment.getProperty(ENDPOINT_PROPERTY, DEFAULT_ENDPOINT);
    List<String> excluded = readExcluded(environment);

    // Beside the endpoint, the OAuth discovery paths leave webforJ too. Whatever the application
    // maps there answers, and an unmapped path answers not found, which is the signal that no
    // authorization is required.
    List<String> required = new ArrayList<>();
    required.add(endpoint + "/**");
    required.addAll(DISCOVERY_EXCLUSIONS);

    boolean changed = false;
    for (String pattern : required) {
      if (!excluded.contains(pattern)) {
        excluded.add(pattern);
        changed = true;
      }
    }

    if (!changed) {
      return;
    }

    for (int index = 0; index < excluded.size(); index++) {
      defaults.put(EXCLUDE_URLS + "[" + index + "]", excluded.get(index));
    }
  }

  private static void deriveComponents(ConfigurableEnvironment environment,
      Map<String, Object> defaults) {
    String components = environment.getProperty(COMPONENTS);
    if (components != null && !components.isBlank()) {
      // The deployment named its component library itself, an explicit value always wins.
      return;
    }

    String origin = environment.getProperty(ORIGIN);
    if (origin == null || origin.isBlank()) {
      return;
    }

    String trimmed = origin.trim();
    while (trimmed.endsWith("/")) {
      trimmed = trimmed.substring(0, trimmed.length() - 1);
    }

    defaults.put(COMPONENTS, trimmed + servletPrefix(environment) + COMPONENTS_SUFFIX);
  }

  private static String servletPrefix(ConfigurableEnvironment environment) {
    String mapping = environment.getProperty(SERVLET_MAPPING, "/*");
    if ("/*".equals(mapping) || "/".equals(mapping) || mapping.isBlank()) {
      // Root mapped deployments serve the webforJ servlet from its internal path.
      return stripWildcard(WebforjServletConfiguration.WEBFORJ_SERVLET_MAPPING);
    }

    return stripWildcard(mapping);
  }

  private static String stripWildcard(String mapping) {
    String prefix = mapping.endsWith("/*") ? mapping.substring(0, mapping.length() - 2) : mapping;
    while (prefix.endsWith("/")) {
      prefix = prefix.substring(0, prefix.length() - 1);
    }

    return prefix.startsWith("/") || prefix.isEmpty() ? prefix : "/" + prefix;
  }

  private static List<String> readExcluded(ConfigurableEnvironment environment) {
    List<String> excluded = new ArrayList<>();

    String single = environment.getProperty(EXCLUDE_URLS);
    if (single != null && !single.isBlank()) {
      for (String value : single.split(",")) {
        if (!value.isBlank()) {
          excluded.add(value.trim());
        }
      }
    }

    for (int index = 0;; index++) {
      String value = environment.getProperty(EXCLUDE_URLS + "[" + index + "]");
      if (value == null) {
        break;
      }

      excluded.add(value);
    }

    return excluded;
  }
}
