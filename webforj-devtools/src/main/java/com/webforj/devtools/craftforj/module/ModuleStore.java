package com.webforj.devtools.craftforj.module;

import com.webforj.devtools.craftforj.action.CraftforjActionException;
import com.webforj.devtools.craftforj.module.model.ModuleSource;
import com.webforj.exceptions.WebforjRuntimeException;
import com.webforj.utilities.Assets;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The craftforJ client modules, read off the classpath and served by name.
 *
 * <p>
 * Only the boot script is injected into the page. Everything else is a module the boot script asks
 * for by name, so the names here are the contract with it, and nothing outside this catalog can be
 * requested. Serving the files as URLs is not an option, since that only works under a servlet
 * container while BBj Services serves htdocs, so the bytes travel over the action channel instead.
 * </p>
 *
 * <p>
 * Each module is base64 encoded once and cached with its SHA-256 hex digest for the JVM lifetime.
 * The digests also travel inline inside the boot script, which is what lets a browser trust a
 * stored copy without asking anything first.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.02
 */
public class ModuleStore {

  /** Where the jar keeps the files served to the app page. */
  public static final String RESOURCE_ROOT = "META-INF/resources/webforj/";

  /** The one script the backend injects, carrying the channel and this catalog's digests. */
  public static final String BOOT_RESOURCE = RESOURCE_ROOT + "craftforj-boot.min.js";

  private static final Map<String, ModuleSource> CACHE = new ConcurrentHashMap<>();

  private final Map<String, String> catalog;

  /**
   * Creates a store over the modules the jar ships.
   */
  public ModuleStore() {
    this(defaultCatalog());
  }

  /**
   * Creates a store over the given catalog.
   *
   * @param catalog the module name mapped to its classpath resource
   */
  public ModuleStore(Map<String, String> catalog) {
    this.catalog = Collections.unmodifiableMap(new LinkedHashMap<>(catalog));
  }

  /**
   * Reads a module by name.
   *
   * @param name the module name
   * @return the base64 payload paired with its digest
   * @throws CraftforjActionException if the name is not in this catalog, or its resource is
   *         unreadable
   */
  public ModuleSource read(String name) {
    String path = catalog.get(name);
    if (path == null) {
      throw new CraftforjActionException("Unknown craftforJ module: " + name);
    }

    return CACHE.computeIfAbsent(path, ModuleStore::readResource);
  }

  /**
   * Builds the digest manifest the boot script carries, one entry per readable module.
   *
   * <p>
   * The manifest reads {@code name:digest,name:digest}, and not JSON, because it is substituted
   * into a string literal inside the built script and minification decides which quotes that
   * literal ends up in. A format carrying no quotes of its own cannot break out of either. Names
   * and hex digests hold no separator, so nothing needs escaping.
   * </p>
   *
   * <p>
   * A module that cannot be read is left out rather than failing the page, so the browser simply
   * never asks for it.
   * </p>
   *
   * @return the manifest, or an empty string when no module can be read
   */
  public String getManifest() {
    StringBuilder manifest = new StringBuilder();
    for (Map.Entry<String, String> entry : catalog.entrySet()) {
      try {
        ModuleSource module = read(entry.getKey());
        if (manifest.length() > 0) {
          manifest.append(',');
        }
        manifest.append(entry.getKey()).append(':').append(module.getSha256());
      } catch (CraftforjActionException e) {
        // The module is not in this build, so the page is told nothing about it.
      }
    }

    return manifest.toString();
  }

  /**
   * The modules the jar ships, in the order they come into play on a page.
   *
   * @return the module name mapped to its classpath resource
   */
  private static Map<String, String> defaultCatalog() {
    Map<String, String> modules = new LinkedHashMap<>();
    modules.put("trigger", RESOURCE_ROOT + "craftforj-trigger.min.js");
    modules.put("gesture", RESOURCE_ROOT + "craftforj-gesture.min.js");
    modules.put("window", RESOURCE_ROOT + "craftforj-window.min.js");
    modules.put("ui", RESOURCE_ROOT + "craftforj-ui.min.js");
    modules.put("agent", RESOURCE_ROOT + "craftforj-agent.min.js");

    return modules;
  }

  /**
   * Reads and base64 encodes a classpath resource.
   *
   * @param path the classpath resource
   * @return the base64 payload paired with its digest
   */
  private static ModuleSource readResource(String path) {
    try {
      String base64 = Assets.contentOf(path, Assets.ContentFormat.BASE64);
      return new ModuleSource(base64, sha256(base64));
    } catch (IllegalArgumentException e) {
      throw new CraftforjActionException("craftforJ module not found on classpath: " + path, e);
    } catch (WebforjRuntimeException e) {
      throw new CraftforjActionException("Failed to read craftforJ module: " + path, e);
    }
  }

  /**
   * Computes the SHA-256 hex digest of the given text.
   *
   * @param text the text to digest
   * @return the hex digest
   */
  private static String sha256(String text) {
    try {
      MessageDigest digest = MessageDigest.getInstance("SHA-256");
      byte[] hash = digest.digest(text.getBytes(StandardCharsets.UTF_8));
      StringBuilder hex = new StringBuilder();
      for (byte b : hash) {
        hex.append(String.format("%02x", b));
      }

      return hex.toString();
    } catch (NoSuchAlgorithmException e) {
      throw new CraftforjActionException("SHA-256 unavailable", e);
    }
  }
}
