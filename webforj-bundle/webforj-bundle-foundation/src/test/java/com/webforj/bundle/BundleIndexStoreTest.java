package com.webforj.bundle;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.jar.JarEntry;
import java.util.jar.JarOutputStream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BundleIndexStoreTest {

  @AfterEach
  void resetCache() {
    BundleIndexStore.reset();
  }

  @Test
  void shouldReadTheIndexFromTheContextClassloader(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"bindings":{"View":["view.js"]}}
        """);

    ClassLoader previous = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader(loader(tmp));
    try {
      BundleIndexStore.reset();

      assertEquals(List.of("view.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(previous);
    }
  }

  @Test
  void shouldReadTheDevelopmentIndexFreshOnEveryCall(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["app.css"]}}
        """);

    ClassLoader previous = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader(loader(tmp));
    try {
      BundleIndexStore.reset();
      assertEquals(List.of("app.css"), BundleIndexStore.get().getBindings().get("View"));

      writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
          {"bindings":{"View":["app.css","styles.scss"]}}
          """);

      assertEquals(List.of("app.css", "styles.scss"),
          BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(previous);
    }
  }

  @Test
  void shouldPreferTheDevelopmentIndexOverTheBuiltIndex(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"bindings":{"View":["built.js"]}}
        """);
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["dev.js"]}}
        """);

    BundleIndex index = BundleIndexStore.read(loader(tmp));

    assertEquals(List.of("dev.js"), index.getBindings().get("View"));
  }

  @Test
  void shouldFallBackToTheBuiltIndexWhenNoDevelopmentIndexIsPresent(@TempDir Path tmp)
      throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"bindings":{"View":["view.js"]},"debug":["panel.js"]}
        """);

    BundleIndex index = BundleIndexStore.read(loader(tmp));

    assertEquals(List.of("view.js"), index.getBindings().get("View"));
    assertEquals(List.of("panel.js"), index.getDebugFiles());
  }

  @Test
  void shouldReadNullWhenNoFileIsPresent(@TempDir Path tmp) {
    assertNull(BundleIndexStore.read(loader(tmp)));
  }

  @Test
  void shouldReadTheEagerBundleFromTheClasspath(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"eager":["generated/bundle-A1B2.js"]}
        """);

    BundleIndex index = BundleIndexStore.read(loader(tmp));

    assertEquals(List.of("generated/bundle-A1B2.js"), index.getEagerFiles());
    assertTrue(index.getBindings().isEmpty());
  }

  @Test
  void shouldKeepTheDevelopmentIndexWhileItsFileIsUnchanged(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["app.css"]}}
        """);
    CountingClassLoader counting = setContextLoader(new CountingClassLoader(tmp.toUri().toURL()));
    try {
      BundleIndex first = BundleIndexStore.get();
      int searches = counting.searches.get();
      for (int i = 0; i < 100; i++) {
        assertSame(first, BundleIndexStore.get());
      }

      assertEquals(searches, counting.searches.get());
    } finally {
      Thread.currentThread().setContextClassLoader(counting.previous);
    }
  }

  @Test
  void shouldReadADevelopmentIndexRewrittenWithTheSameSizeAndTime(@TempDir Path tmp)
      throws IOException {
    Path file = tmp.resolve(BundleIndexDocument.DEVELOPMENT_RESOURCE);
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["app-A1B2.css"]}}
        """);
    FileTime written = Files.getLastModifiedTime(file);
    CountingClassLoader counting = setContextLoader(new CountingClassLoader(tmp.toUri().toURL()));
    try {
      assertEquals(List.of("app-A1B2.css"), BundleIndexStore.get().getBindings().get("View"));

      writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
          {"bindings":{"View":["app-C3D4.css"]}}
          """);
      Files.setLastModifiedTime(file, written);

      assertEquals(List.of("app-C3D4.css"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(counting.previous);
    }
  }

  @Test
  void shouldSearchOnceWhenTheClasspathHasArchivesOnly() throws IOException {
    CountingClassLoader counting = setContextLoader(
        new CountingClassLoader(Gson.class.getProtectionDomain().getCodeSource().getLocation()));
    try {
      for (int i = 0; i < 100; i++) {
        BundleIndexStore.get();
      }

      assertEquals(1, counting.searches.get());
    } finally {
      Thread.currentThread().setContextClassLoader(counting.previous);
    }
  }

  @Test
  void shouldReadADevelopmentIndexWrittenLaterWithoutSearchingBefore(@TempDir Path tmp)
      throws IOException {
    CountingClassLoader counting = setContextLoader(new CountingClassLoader(tmp.toUri().toURL()));
    try {
      for (int i = 0; i < 100; i++) {
        assertNull(BundleIndexStore.get());
      }

      assertEquals(1, counting.searches.get());

      writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
          {"bindings":{"View":["late.js"]}}
          """);

      assertEquals(List.of("late.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(counting.previous);
    }
  }

  @Test
  void shouldReadADevelopmentIndexThatWasDeletedAndWrittenAgain(@TempDir Path tmp)
      throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"bindings":{"View":["built.js"]}}
        """);
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["dev.js"]}}
        """);
    CountingClassLoader counting = setContextLoader(new CountingClassLoader(tmp.toUri().toURL()));
    try {
      assertEquals(List.of("dev.js"), BundleIndexStore.get().getBindings().get("View"));

      Files.delete(tmp.resolve(BundleIndexDocument.DEVELOPMENT_RESOURCE));
      assertEquals(List.of("built.js"), BundleIndexStore.get().getBindings().get("View"));

      writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
          {"bindings":{"View":["again.js"]}}
          """);
      assertEquals(List.of("again.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(counting.previous);
    }
  }

  @Test
  void shouldSearchOnEveryCallWhenTheClasspathCannotBeListed(@TempDir Path tmp) throws IOException {
    ClassLoader previous = Thread.currentThread().getContextClassLoader();
    UnlistedClassLoader unlisted = new UnlistedClassLoader(tmp);
    Thread.currentThread().setContextClassLoader(unlisted);
    try {
      BundleIndexStore.reset();
      assertNull(BundleIndexStore.get());
      int searches = unlisted.searches.get();
      for (int i = 0; i < 10; i++) {
        assertNull(BundleIndexStore.get());
      }

      assertEquals(searches + 10, unlisted.searches.get());

      writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
          {"bindings":{"View":["late.js"]}}
          """);

      assertEquals(List.of("late.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(previous);
    }
  }

  @Test
  void shouldReadADevelopmentIndexThatAppearsEarlierOnTheClasspath(@TempDir Path first,
      @TempDir Path second) throws IOException {
    writeIndex(second, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["second.js"]}}
        """);
    ClassLoader previous = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader(
        new URLClassLoader(new URL[] {first.toUri().toURL(), second.toUri().toURL()},
            ClassLoader.getPlatformClassLoader()));
    try {
      BundleIndexStore.reset();
      assertEquals(List.of("second.js"), BundleIndexStore.get().getBindings().get("View"));

      writeIndex(first, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
          {"bindings":{"View":["first.js"]}}
          """);

      assertEquals(List.of("first.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(previous);
    }
  }

  @Test
  void shouldFallBackToTheBuiltIndexWhileTheDevelopmentIndexCannotBeRead(@TempDir Path tmp)
      throws IOException {
    writeIndex(tmp, BundleIndexDocument.RESOURCE, """
        {"bindings":{"View":["built.js"]}}
        """);
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["dev.js"]}}
        """);
    Path file = tmp.resolve(BundleIndexDocument.DEVELOPMENT_RESOURCE);
    CountingClassLoader counting = setContextLoader(new CountingClassLoader(tmp.toUri().toURL()));
    try {
      assertEquals(List.of("dev.js"), BundleIndexStore.get().getBindings().get("View"));

      Files.delete(file);
      Files.createDirectory(file);

      assertEquals(List.of("built.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(counting.previous);
    }
  }

  @Test
  void shouldReadADevelopmentIndexAfterAMalformedOne(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, "{\"bindings\":{\"View\":[");
    CountingClassLoader counting = setContextLoader(new CountingClassLoader(tmp.toUri().toURL()));
    try {
      assertThrows(JsonSyntaxException.class, BundleIndexStore::get);

      writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
          {"bindings":{"View":["fixed.js"]}}
          """);

      assertEquals(List.of("fixed.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(counting.previous);
    }
  }

  @Test
  void shouldReadADevelopmentIndexInsideAnArchive(@TempDir Path tmp) throws IOException {
    Path archive = tmp.resolve("app.jar");
    try (JarOutputStream jar = new JarOutputStream(Files.newOutputStream(archive))) {
      jar.putNextEntry(new JarEntry(BundleIndexDocument.DEVELOPMENT_RESOURCE));
      jar.write("{\"bindings\":{\"View\":[\"packed.js\"]}}".getBytes(StandardCharsets.UTF_8));
      jar.closeEntry();
    }

    CountingClassLoader counting =
        setContextLoader(new CountingClassLoader(archive.toUri().toURL()));
    try {
      assertEquals(List.of("packed.js"), BundleIndexStore.get().getBindings().get("View"));
      assertEquals(List.of("packed.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(counting.previous);
      counting.close();
    }
  }

  @Test
  void shouldSearchTheClasspathWhileItCannotBeListed(@TempDir Path tmp) throws IOException {
    writeIndex(tmp, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["app.js"]}}
        """);
    CountingClassLoader counting = setContextLoader(new CountingClassLoader(tmp.toUri().toURL()));
    try {
      Thread.currentThread().interrupt();
      BundleIndex interrupted = BundleIndexStore.get();
      assertTrue(Thread.interrupted());

      assertEquals(List.of("app.js"), interrupted.getBindings().get("View"));
      assertEquals(List.of("app.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.interrupted();
      Thread.currentThread().setContextClassLoader(counting.previous);
    }
  }

  @Test
  void shouldKeepTheDevelopmentIndexOfEachClassLoaderApart(@TempDir Path first,
      @TempDir Path second) throws IOException {
    writeIndex(first, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["first.js"]}}
        """);
    writeIndex(second, BundleIndexDocument.DEVELOPMENT_RESOURCE, """
        {"bindings":{"View":["second.js"]}}
        """);
    CountingClassLoader one = setContextLoader(new CountingClassLoader(first.toUri().toURL()));
    ClassLoader two = loader(second);
    try {
      assertEquals(List.of("first.js"), BundleIndexStore.get().getBindings().get("View"));

      Thread.currentThread().setContextClassLoader(two);
      assertEquals(List.of("second.js"), BundleIndexStore.get().getBindings().get("View"));

      Thread.currentThread().setContextClassLoader(one);
      assertEquals(List.of("first.js"), BundleIndexStore.get().getBindings().get("View"));
    } finally {
      Thread.currentThread().setContextClassLoader(one.previous);
    }
  }

  private static CountingClassLoader setContextLoader(CountingClassLoader counting) {
    Thread.currentThread().setContextClassLoader(counting);
    BundleIndexStore.reset();

    return counting;
  }

  private static void writeIndex(Path root, String resource, String json) throws IOException {
    Path target = root.resolve(resource);
    Files.createDirectories(target.getParent());
    Files.writeString(target, json);
  }

  private static ClassLoader loader(Path root) {
    try {
      return new URLClassLoader(new URL[] {root.toUri().toURL()},
          ClassLoader.getPlatformClassLoader());
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  static final class CountingClassLoader extends URLClassLoader {
    private final AtomicInteger searches = new AtomicInteger();
    private final ClassLoader previous = Thread.currentThread().getContextClassLoader();

    CountingClassLoader(URL entry) {
      super(new URL[] {entry}, ClassLoader.getPlatformClassLoader());
    }

    @Override
    public URL findResource(String name) {
      if (BundleIndexDocument.DEVELOPMENT_RESOURCE.equals(name)) {
        searches.incrementAndGet();
      }

      return super.findResource(name);
    }
  }

  static final class UnlistedClassLoader extends ClassLoader {
    private final AtomicInteger searches = new AtomicInteger();
    private final Path root;

    UnlistedClassLoader(Path root) {
      super(ClassLoader.getPlatformClassLoader());
      this.root = root;
    }

    @Override
    protected URL findResource(String name) {
      if (BundleIndexDocument.DEVELOPMENT_RESOURCE.equals(name)) {
        searches.incrementAndGet();
      }

      Path file = root.resolve(name);
      try {
        return Files.isRegularFile(file) ? file.toUri().toURL() : null;
      } catch (IOException e) {
        return null;
      }
    }
  }
}
