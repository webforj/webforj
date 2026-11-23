package com.webforj.bundle;

import com.google.gson.Gson;
import io.github.classgraph.ClassGraph;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.lang.ref.WeakReference;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

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
 * <p>
 * The development index is current on every call. A classpath made of archives only cannot receive
 * one while the application runs and is searched for it once.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.01
 */
public final class BundleIndexStore {

  private static final Gson GSON = new Gson();
  private static final AtomicReference<DevelopmentIndex[]> DEVELOPMENT =
      new AtomicReference<>(new DevelopmentIndex[0]);

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

    BundleIndex development = readDevelopment(classLoader);
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
    DEVELOPMENT.set(new DevelopmentIndex[0]);
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

      return parse(stream);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static BundleIndex parse(InputStream stream) throws IOException {
    try (Reader reader = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
      BundleIndexDocument document = GSON.fromJson(reader, BundleIndexDocument.class);

      return document == null ? null : document.toIndex();
    }
  }

  /**
   * Reads the development index of the class loader.
   *
   * <p>
   * The classpath is searched unless the index is a file that is known, or it is known where on the
   * classpath one can appear.
   * </p>
   */
  private static BundleIndex readDevelopment(ClassLoader classLoader) {
    DevelopmentIndex known = findDevelopment(classLoader);
    if (known == null) {
      known = createDevelopment(classLoader, findPlaces(classLoader));
    } else if (known.mode != DevelopmentIndex.Mode.SEARCHED && !known.isCurrent()) {
      known = createDevelopment(classLoader, known.places);
    }

    return known.mode == DevelopmentIndex.Mode.SEARCHED
        ? read(classLoader, BundleIndexDocument.DEVELOPMENT_RESOURCE)
        : known.index;
  }

  private static DevelopmentIndex findDevelopment(ClassLoader classLoader) {
    for (DevelopmentIndex development : DEVELOPMENT.get()) {
      ClassLoader owner = development.classLoader.get();
      if (owner != null && owner == classLoader) {
        return development;
      }
    }

    return null;
  }

  /**
   * Creates and saves what is known of the development index of the class loader.
   *
   * @param places the places an index can appear at, or {@code null} when they are not known
   */
  private static DevelopmentIndex createDevelopment(ClassLoader classLoader, File[] places) {
    URL url = classLoader.getResource(BundleIndexDocument.DEVELOPMENT_RESOURCE);
    File file = url == null ? null : toFile(url);
    if (places == null || (url != null && file == null)) {
      return saveDevelopment(
          new DevelopmentIndex(classLoader, DevelopmentIndex.Mode.SEARCHED, places, null, null));
    }

    File[] watched = file == null ? places : addPlace(places, file);
    DevelopmentIndex absent =
        new DevelopmentIndex(classLoader, DevelopmentIndex.Mode.ABSENT, watched, null, null);
    byte[] content = file == null ? null : readContent(file);
    if (content == null) {
      return saveDevelopment(absent);
    }

    try {
      return saveDevelopment(new DevelopmentIndex(classLoader, DevelopmentIndex.Mode.FOLLOWED,
          watched, file, content));
    } catch (RuntimeException e) {
      saveDevelopment(absent);
      throw e;
    }
  }

  /**
   * Finds the places a development index can be written to, which are the index file in every
   * folder of the classpath.
   *
   * @return the places in classpath order, or {@code null} when the classpath cannot be listed
   */
  private static File[] findPlaces(ClassLoader classLoader) {
    List<File> entries;
    try {
      entries = new ClassGraph().overrideClassLoaders(classLoader)
          .filterClasspathElements(
              element -> !element.startsWith(System.getProperty("basis.BBjHome") + "/.lib"))
          .getClasspathFiles();
    } catch (RuntimeException e) {
      return null;
    }

    if (entries.isEmpty()) {
      return null;
    }

    List<File> places = new ArrayList<>();
    for (File entry : entries) {
      if (entry.isDirectory()) {
        places.add(new File(entry, BundleIndexDocument.DEVELOPMENT_RESOURCE));
      }
    }

    return places.toArray(new File[0]);
  }

  private static File[] addPlace(File[] places, File file) {
    for (File place : places) {
      if (isSameFile(place, file)) {
        return places;
      }
    }

    File[] extended = Arrays.copyOf(places, places.length + 1);
    extended[places.length] = file;

    return extended;
  }

  private static boolean isSameFile(File place, File file) {
    try {
      return place.isFile() && Files.isSameFile(place.toPath(), file.toPath());
    } catch (IOException e) {
      return false;
    }
  }

  private static DevelopmentIndex saveDevelopment(DevelopmentIndex created) {
    ClassLoader classLoader = created.classLoader.get();
    DevelopmentIndex[] known;
    DevelopmentIndex[] updated;
    do {
      known = DEVELOPMENT.get();
      List<DevelopmentIndex> kept = new ArrayList<>(known.length + 1);
      for (DevelopmentIndex development : known) {
        ClassLoader owner = development.classLoader.get();
        if (owner != null && owner != classLoader) {
          kept.add(development);
        }
      }

      kept.add(created);
      updated = kept.toArray(new DevelopmentIndex[0]);
    } while (!DEVELOPMENT.compareAndSet(known, updated));

    return created;
  }

  private static File toFile(URL url) {
    if (!"file".equals(url.getProtocol())) {
      return null;
    }

    try {
      return new File(url.toURI());
    } catch (URISyntaxException | IllegalArgumentException e) {
      return null;
    }
  }

  private static byte[] readContent(File file) {
    try {
      return Files.readAllBytes(file.toPath());
    } catch (IOException e) {
      return null;
    }
  }

  private static BundleIndex parseContent(byte[] content) {
    try {
      return parse(new ByteArrayInputStream(content));
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  /**
   * The development index of one class loader as it was last read.
   */
  private static final class DevelopmentIndex {
    private final WeakReference<ClassLoader> classLoader;
    private final Mode mode;
    private final File[] places;
    private final File file;
    private final byte[] content;
    private final BundleIndex index;

    DevelopmentIndex(ClassLoader classLoader, Mode mode, File[] places, File file, byte[] content) {
      this.classLoader = new WeakReference<>(classLoader);
      this.mode = mode;
      this.places = places;
      this.file = file;
      this.content = content;
      this.index = content == null ? null : parseContent(content);
    }

    /**
     * Checks whether the file system still shows what was read.
     */
    boolean isCurrent() {
      for (File place : places) {
        if (place.isFile()) {
          return file != null && isSameFile(place, file)
              && Arrays.equals(content, readContent(file));
        }
      }

      return file == null;
    }

    /**
     * How the development index of a class loader is kept current.
     */
    enum Mode {
      /** No index file, the places one can appear at are watched. */
      ABSENT,
      /** The file of the index is read on every call. */
      FOLLOWED,
      /** The classpath is searched on every call. */
      SEARCHED
    }
  }
}
