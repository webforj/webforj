package com.webforj.mcp;

import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletRegistration;
import java.util.Iterator;

/**
 * Resolves the path prefix the webforJ servlet answers on.
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public final class McpAppServletPath {

  private static final String WEBFORJ_SERVLET_CLASS = "com.webforj.servlet.WebforjServlet";

  private McpAppServletPath() {
    // Utility class
  }

  /**
   * Reads the prefix out of the servlet registrations of a deployment.
   *
   * <p>
   * A deployment that serves webforJ from the root contributes an empty prefix, and one that remaps
   * the servlet to a sub path contributes that path, so the app page addresses the embed bootstrap
   * wherever the application put it.
   * </p>
   *
   * @param context the servlet context of the deployment
   * @return the prefix, empty when webforJ answers on the root
   */
  public static String of(ServletContext context) {
    if (context == null) {
      return "";
    }

    for (ServletRegistration registration : context.getServletRegistrations().values()) {
      if (!WEBFORJ_SERVLET_CLASS.equals(registration.getClassName())) {
        continue;
      }

      Iterator<String> mappings = registration.getMappings().iterator();
      if (mappings.hasNext()) {
        return normalize(mappings.next());
      }
    }

    return "";
  }

  /**
   * Turns a servlet mapping into a path prefix.
   *
   * @param mapping the servlet mapping
   * @return the prefix, empty when the mapping covers the root
   */
  public static String normalize(String mapping) {
    if (mapping == null || mapping.isBlank() || "/*".equals(mapping) || "/".equals(mapping)) {
      return "";
    }

    String prefix = mapping.endsWith("/*") ? mapping.substring(0, mapping.length() - 2) : mapping;
    while (prefix.endsWith("/")) {
      prefix = prefix.substring(0, prefix.length() - 1);
    }

    if (prefix.isEmpty()) {
      return "";
    }

    return prefix.startsWith("/") ? prefix : "/" + prefix;
  }
}
