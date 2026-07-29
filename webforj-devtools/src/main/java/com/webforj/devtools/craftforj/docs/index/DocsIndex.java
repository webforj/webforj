package com.webforj.devtools.craftforj.docs.index;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import com.webforj.devtools.craftforj.docs.model.DocsEntry;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Type;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Map;

/**
 * Index for component documentation loaded from docs-index.json.
 *
 * <p>
 * This class loads the pre-built documentation index from the classpath and provides lookup methods
 * for finding documentation by server or client component name.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class DocsIndex {

  private static final String INDEX_PATH = "META-INF/resources/docs-index.json";
  private final Map<String, DocsEntry> entries;

  /**
   * Creates a new DocsIndex, loading entries from the classpath.
   */
  public DocsIndex() {
    this.entries = loadIndex();
  }

  /**
   * Looks up documentation by server component class name.
   *
   * @param className the fully qualified class name (e.g., "com.webforj.component.button.Button")
   * @return the docs entry, or null if not found
   */
  public DocsEntry findByServerComponent(String className) {
    if (className == null || className.isEmpty()) {
      return null;
    }

    return entries.get(className);
  }

  /**
   * Looks up documentation by client component tag name.
   *
   * @param tagName the tag name (e.g., "dwc-button")
   * @return the docs entry, or null if not found
   */
  public DocsEntry findByClientComponent(String tagName) {
    if (tagName == null || tagName.isEmpty()) {
      return null;
    }
    for (DocsEntry entry : entries.values()) {
      if (tagName.equals(entry.getClientComponent())) {
        return entry;
      }
    }

    return null;
  }

  /**
   * Gets all entries in the index.
   *
   * @return unmodifiable map of class name to docs entry
   */
  public Map<String, DocsEntry> getEntries() {
    return Collections.unmodifiableMap(entries);
  }

  private Map<String, DocsEntry> loadIndex() {
    try (InputStream is = getClass().getClassLoader().getResourceAsStream(INDEX_PATH)) {
      if (is == null) {
        return Collections.emptyMap();
      }
      Type type = new TypeToken<Map<String, DocsEntry>>() {}.getType();
      Map<String, DocsEntry> loaded =
          new Gson().fromJson(new InputStreamReader(is, StandardCharsets.UTF_8), type);

      // Gson returns null (no exception) for an empty stream or literal null

      return loaded != null ? loaded : Collections.emptyMap();
    } catch (Exception e) {
      return Collections.emptyMap();
    }
  }
}
