package com.webforj.devtools.craftforj.docs.resolver;

import com.webforj.devtools.craftforj.docs.model.DocsQuery;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.stream.Collectors;

/**
 * Resolver for custom documentation in META-INF/docs/custom/.
 *
 * <p>
 * Looks for markdown files matching the server or client component name.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class CustomDocsResolver implements DocsResolver {

  private static final String CUSTOM_DOCS_PATH = "META-INF/docs/";

  /**
   * {@inheritDoc}
   */
  @Override
  public String resolve(DocsQuery query) {
    // Try server component name first
    String markdown = tryLoadCustomDocs(query.getServerComponent());
    if (markdown != null) {
      return markdown;
    }

    // Try client component name
    return tryLoadCustomDocs(query.getClientComponent());
  }

  private String tryLoadCustomDocs(String name) {
    if (name == null || name.isEmpty()) {
      return null;
    }

    String path = CUSTOM_DOCS_PATH + name + ".md";
    try (InputStream is = getClass().getClassLoader().getResourceAsStream(path)) {
      if (is == null) {
        return null;
      }
      try (BufferedReader reader =
          new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))) {
        return reader.lines().collect(Collectors.joining("\n"));
      }
    } catch (Exception e) {
      return null;
    }
  }
}
