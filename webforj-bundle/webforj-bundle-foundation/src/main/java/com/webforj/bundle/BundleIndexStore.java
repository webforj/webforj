package com.webforj.bundle;

import com.google.gson.Gson;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;

/**
 * Reads the bundle index the runtime resolves outputs against.
 *
 * <p>
 * The index always lives on disk. A built application reads the index file the build emitted under
 * {@code META-INF}. A development application reads the index the watch emitted under the static
 * folder, which the development servers exclude from their reload scan so refreshing it never
 * redeploys the application. The development index is read first, so it supersedes a stale built
 * index left next to it.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundleIndexStore {

  private static final Gson GSON = new Gson();

  private static volatile BundleIndex onDisk; // NOSONAR
  private static volatile boolean diskRead;

  private BundleIndexStore() {}

  /**
   * Gets the index read from disk.
   *
   * @return the current index, or {@code null} when no index file is on the classpath
   */
  public static BundleIndex get() {
    ClassLoader classLoader = Thread.currentThread().getContextClassLoader();

    BundleIndex development = read(classLoader, BundleIndexDocument.DEVELOPMENT_RESOURCE);
    if (development != null) {
      return development;
    }

    if (!diskRead) {
      synchronized (BundleIndexStore.class) {
        if (!diskRead) {
          onDisk = read(classLoader, BundleIndexDocument.RESOURCE);
          diskRead = true;
        }
      }
    }

    return onDisk;
  }

  /**
   * Discards the cached index so the next {@link #get()} reads it again.
   */
  static void reset() {
    onDisk = null;
    diskRead = false;
  }

  /**
   * Reads the index file from the given classloader, the development index first, then the built
   * index.
   *
   * @param classLoader the classloader to read the index file from
   * @return the index, or {@code null} when no file is present
   */
  static BundleIndex read(ClassLoader classLoader) {
    BundleIndex development = read(classLoader, BundleIndexDocument.DEVELOPMENT_RESOURCE);

    return development != null ? development : read(classLoader, BundleIndexDocument.RESOURCE);
  }

  private static BundleIndex read(ClassLoader classLoader, String resource) {
    try (InputStream stream = classLoader.getResourceAsStream(resource)) {
      if (stream == null) {
        return null;
      }

      try (Reader reader = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
        BundleIndexDocument document = GSON.fromJson(reader, BundleIndexDocument.class);

        return document == null ? null : document.toIndex();
      }
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }
}
