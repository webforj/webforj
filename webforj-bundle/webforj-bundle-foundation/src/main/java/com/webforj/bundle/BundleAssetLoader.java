package com.webforj.bundle;

import com.webforj.Environment;
import com.webforj.Page;
import com.webforj.environment.ObjectTable;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Function;

/**
 * Resolves the bundle outputs bound to a class and injects them into the page, once per session.
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
class BundleAssetLoader {
  private static final String URL_PREFIX = "ws://frontend/";
  private static final String CONTEXT_PREFIX = "context://static/frontend/";
  private static final String LOADED_KEY_PREFIX = "com.webforj.bundle.BundleAssetLoader::loaded::";

  private final BundleIndex fixedIndex;

  /**
   * Creates a loader that reads the current index from the {@link BundleIndexStore}.
   */
  BundleAssetLoader() {
    this.fixedIndex = null;
  }

  /**
   * Creates a loader with an explicit index.
   *
   * @param index the index to resolve outputs against
   */
  BundleAssetLoader(BundleIndex index) {
    this.fixedIndex = index;
  }

  /**
   * Resolves the outputs bound to the given class and inject them into the page, once per session.
   *
   * @param boundClass the class whose bound outputs are loaded
   */
  void loadFor(Class<?> boundClass) {
    List<String> urls = resolveUrlsFor(boundClass);
    if (urls.isEmpty()) {
      return;
    }

    Page.ifPresent(page -> {
      for (String url : urls) {
        String key = LOADED_KEY_PREFIX + url;
        if (ObjectTable.contains(key)) {
          continue;
        }

        ObjectTable.put(key, Boolean.TRUE);
        if (isStylesheet(url)) {
          page.addStyleSheet(url, false, Map.of());
        } else {
          page.addJavaScript(url, false, Map.of("type", "module"));
        }
      }
    });
  }

  /**
   * Loads the single eager bundle into the page once per session, when the build produced one.
   *
   * <p>
   * When the runtime serves no static folder of its own, the bundle is read from the classpath and
   * inlined, since there is no web server to serve it. Otherwise it is served from the static
   * folder the same as the per component outputs.
   * </p>
   */
  void loadEager() {
    injectOnce("eager::", BundleIndex::getEagerFiles);
  }

  /**
   * Loads the global outputs into the page once per session, independent of the routed class.
   *
   * <p>
   * These are the assets a plugin contributes for the whole application, such as a compiled utility
   * stylesheet. When the runtime serves no static folder of its own, they are read from the
   * classpath and inlined, since there is no web server to serve them. Otherwise they are served
   * from the static folder.
   * </p>
   */
  void loadGlobal() {
    injectOnce("global::", BundleIndex::getGlobalFiles);
  }

  private void injectOnce(String keyspace, Function<BundleIndex, List<String>> select) {
    BundleIndex index = index();
    if (index == null) {
      return;
    }

    List<String> files = select.apply(index);
    if (files.isEmpty()) {
      return;
    }

    Page.ifPresent(page -> {
      for (String file : files) {
        String key = LOADED_KEY_PREFIX + keyspace + file;
        if (ObjectTable.contains(key)) {
          continue;
        }

        ObjectTable.put(key, Boolean.TRUE);
        inject(page, file);
      }
    });
  }

  private void inject(Page page, String file) {
    boolean stylesheet = isStylesheet(file);
    if (Environment.isRunningWithBBjServices()) {
      String url = CONTEXT_PREFIX + file;
      if (stylesheet) {
        page.addInlineStyleSheet(url, false, Map.of());
      } else {
        page.addInlineJavaScript(url, false, Map.of("type", "module"));
      }
    } else {
      String url = URL_PREFIX + file;
      if (stylesheet) {
        page.addStyleSheet(url, false, Map.of());
      } else {
        page.addJavaScript(url, false, Map.of("type", "module"));
      }
    }
  }

  List<String> resolveUrlsFor(Class<?> boundClass) {
    return resolveUrlsFor(boundClass, isDebugMode());
  }

  List<String> resolveUrlsFor(Class<?> boundClass, boolean debug) {
    BundleIndex index = index();
    if (index == null) {
      return List.of();
    }

    List<String> files = index.getBindings().get(boundClass.getName());
    if (files == null || files.isEmpty()) {
      return List.of();
    }

    List<String> debugFiles = index.getDebugFiles();
    List<String> urls = new ArrayList<>();
    for (String file : files) {
      if (!debug && debugFiles.contains(file)) {
        continue;
      }

      urls.add(URL_PREFIX + file);
    }

    return List.copyOf(urls);
  }

  private BundleIndex index() {
    return fixedIndex != null ? fixedIndex : BundleIndexStore.get();
  }

  private static boolean isDebugMode() {
    Environment environment = Environment.getCurrent();

    return environment != null && environment.isDebug();
  }

  private static boolean isStylesheet(String url) {
    return url.toLowerCase(Locale.ROOT).endsWith(".css");
  }
}
