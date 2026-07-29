package com.webforj.devtools.craftforj.docs.fetcher;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.webforj.devtools.craftforj.docs.model.DwcStylingData;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Fetches DWC styling data from dwc.style.
 *
 * <p>
 * This class fetches component styling information (parts, CSS properties, slots, etc.) from the
 * DWC documentation site and caches the results.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class DwcFetcher {

  private static final String DWC_COMPONENTS_URL = "https://dwc.style/docs/dwc-components.json";
  private static final int TIMEOUT_MS = 5000;
  private static final int MAX_RESPONSE_BYTES = 5 * 1024 * 1024;
  private static final long OVERALL_DEADLINE_MS = 15_000;
  private static final long RETRY_INTERVAL_MS = 60_000;

  // JSON field names
  private static final String FIELD_TAG = "tag";
  private static final String FIELD_COMPONENTS = "components";
  private static final String FIELD_PARTS = "parts";
  private static final String FIELD_CSS_PROPERTIES = "styles";
  private static final String FIELD_SLOTS = "slots";
  private static final String FIELD_ATTRIBUTES = "props";
  private static final String FIELD_DEPENDENCIES = "dependencies";
  private static final String FIELD_NAME = "name";
  private static final String FIELD_DOCS = "docs";
  private static final String FIELD_TYPE = "type";
  private static final String FIELD_REFLECTS = "reflectToAttr";

  private final Map<String, DwcStylingData> cache = new ConcurrentHashMap<>();
  private final Set<String> notFound = ConcurrentHashMap.newKeySet();
  private final String componentsUrl;
  private JsonArray componentsData;
  private long lastFailedFetchTime;

  /**
   * Creates a fetcher against the public DWC documentation site.
   */
  public DwcFetcher() {
    this(DWC_COMPONENTS_URL);
  }

  /**
   * Creates a fetcher against a specific components URL.
   *
   * @param componentsUrl the URL serving the components JSON
   */
  DwcFetcher(String componentsUrl) {
    this.componentsUrl = componentsUrl;
  }

  /**
   * Fetches styling data for a component.
   *
   * @param tagName the component tag name (e.g., "dwc-button")
   * @return the styling data, or null if not found
   */
  public DwcStylingData fetch(String tagName) {
    if (tagName == null || tagName.isEmpty()) {
      return null;
    }

    // Check cache first
    if (cache.containsKey(tagName)) {
      return cache.get(tagName);
    }
    if (notFound.contains(tagName)) {
      return null;
    }

    // Ensure we have the components data
    ensureComponentsLoaded();

    if (componentsData == null) {
      return null;
    }

    // Find the component in the data
    for (JsonElement element : componentsData) {
      JsonObject component = element.getAsJsonObject();
      String tag = component.has(FIELD_TAG) ? component.get(FIELD_TAG).getAsString() : null;
      if (tagName.equals(tag)) {
        DwcStylingData data = parseComponent(component);
        cache.put(tagName, data);

        return data;
      }
    }

    // Not found
    notFound.add(tagName);

    return null;
  }

  private synchronized void ensureComponentsLoaded() {
    if (componentsData != null) {
      return;
    }

    // A failed fetch is retried after a cooldown instead of latching for the JVM lifetime,
    // so an app started offline gets DWC data once the network is back
    long start = System.currentTimeMillis();
    if (lastFailedFetchTime != 0 && start - lastFailedFetchTime < RETRY_INTERVAL_MS) {
      return;
    }

    try {
      HttpURLConnection conn =
          (HttpURLConnection) URI.create(componentsUrl).toURL().openConnection();
      conn.setConnectTimeout(TIMEOUT_MS);
      conn.setReadTimeout(TIMEOUT_MS);
      conn.setRequestProperty("Accept", "application/json");

      if (conn.getResponseCode() == 200) {
        String body = readBounded(conn, start + OVERALL_DEADLINE_MS);
        JsonObject root = JsonParser.parseString(body).getAsJsonObject();
        if (root.has(FIELD_COMPONENTS)) {
          componentsData = root.getAsJsonArray(FIELD_COMPONENTS);
        }
      }
    } catch (Exception e) {
      // Silently fail - DWC data is optional
    }

    if (componentsData == null) {
      lastFailedFetchTime = System.currentTimeMillis();
    }
  }

  private String readBounded(HttpURLConnection conn, long deadline) throws IOException {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    byte[] buffer = new byte[8192];

    // The per-read timeout does not bound a slow-drip response; the size cap and the overall
    // deadline do
    try (InputStream in = conn.getInputStream()) {
      int read;
      while ((read = in.read(buffer)) != -1) {
        out.write(buffer, 0, read);
        if (out.size() > MAX_RESPONSE_BYTES) {
          throw new IOException("DWC components response too large");
        }
        if (System.currentTimeMillis() > deadline) {
          throw new IOException("DWC components fetch deadline exceeded");
        }
      }
    }

    return out.toString(StandardCharsets.UTF_8);
  }

  private DwcStylingData parseComponent(JsonObject component) {
    List<DwcStylingData.Part> parts = null;
    List<DwcStylingData.CssProperty> cssProperties = null;
    List<DwcStylingData.Slot> slots = null;
    List<DwcStylingData.ReflectedAttribute> reflects = null;
    List<String> dependencies = null;

    // Parse parts
    if (component.has(FIELD_PARTS) && component.get(FIELD_PARTS).isJsonArray()) {
      List<DwcStylingData.Part> list = new ArrayList<>();
      for (JsonElement el : component.getAsJsonArray(FIELD_PARTS)) {
        JsonObject obj = el.getAsJsonObject();
        String name = getStringOrNull(obj, FIELD_NAME);
        if (name != null) {
          list.add(new DwcStylingData.Part(name, getStringOrNull(obj, FIELD_DOCS)));
        }
      }
      if (!list.isEmpty()) {
        parts = list;
      }
    }

    // Parse CSS properties
    if (component.has(FIELD_CSS_PROPERTIES) && component.get(FIELD_CSS_PROPERTIES).isJsonArray()) {
      List<DwcStylingData.CssProperty> list = new ArrayList<>();
      for (JsonElement el : component.getAsJsonArray(FIELD_CSS_PROPERTIES)) {
        JsonObject obj = el.getAsJsonObject();
        String name = getStringOrNull(obj, FIELD_NAME);
        if (name != null) {
          list.add(new DwcStylingData.CssProperty(name, getStringOrNull(obj, FIELD_DOCS)));
        }
      }
      if (!list.isEmpty()) {
        cssProperties = list;
      }
    }

    // Parse slots
    if (component.has(FIELD_SLOTS) && component.get(FIELD_SLOTS).isJsonArray()) {
      List<DwcStylingData.Slot> list = new ArrayList<>();
      for (JsonElement el : component.getAsJsonArray(FIELD_SLOTS)) {
        JsonObject obj = el.getAsJsonObject();
        list.add(new DwcStylingData.Slot(getStringOrNull(obj, FIELD_NAME),
            getStringOrNull(obj, FIELD_DOCS)));
      }
      if (!list.isEmpty()) {
        slots = list;
      }
    }

    // Parse reflected attributes
    if (component.has(FIELD_ATTRIBUTES) && component.get(FIELD_ATTRIBUTES).isJsonArray()) {
      List<DwcStylingData.ReflectedAttribute> list = new ArrayList<>();
      for (JsonElement el : component.getAsJsonArray(FIELD_ATTRIBUTES)) {
        JsonObject obj = el.getAsJsonObject();
        // Only include reflected attributes
        if (obj.has(FIELD_REFLECTS) && obj.get(FIELD_REFLECTS).getAsBoolean()) {
          String name = getStringOrNull(obj, FIELD_NAME);
          if (name != null) {
            list.add(new DwcStylingData.ReflectedAttribute(name, getStringOrNull(obj, FIELD_TYPE),
                getStringOrNull(obj, FIELD_DOCS)));
          }
        }
      }
      if (!list.isEmpty()) {
        reflects = list;
      }
    }

    // Parse dependencies
    if (component.has(FIELD_DEPENDENCIES) && component.get(FIELD_DEPENDENCIES).isJsonArray()) {
      List<String> list = new ArrayList<>();
      for (JsonElement el : component.getAsJsonArray(FIELD_DEPENDENCIES)) {
        if (el.isJsonPrimitive()) {
          list.add(el.getAsString());
        }
      }
      if (!list.isEmpty()) {
        dependencies = list;
      }
    }

    return new DwcStylingData(parts, cssProperties, slots, reflects, dependencies);
  }

  private String getStringOrNull(JsonObject obj, String key) {
    if (obj.has(key) && !obj.get(key).isJsonNull()) {
      return obj.get(key).getAsString();
    }

    return null;
  }

  /**
   * Clears the cache.
   */
  public void clearCache() {
    cache.clear();
    notFound.clear();
    componentsData = null;
    lastFailedFetchTime = 0;
  }
}
