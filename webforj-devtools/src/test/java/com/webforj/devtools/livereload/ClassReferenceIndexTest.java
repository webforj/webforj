package com.webforj.devtools.livereload;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.net.URI;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.util.Set;
import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ClassReferenceIndexTest {

  @TempDir
  Path classes;

  private ClassReferenceIndex index;
  private URLClassLoader loader;

  @BeforeEach
  void setUp() throws IOException {
    index = new ClassReferenceIndex();
    loader = newLoader(classes.toUri().toURL());
  }

  @AfterEach
  void tearDown() throws IOException {
    loader.close();
  }

  @Test
  void shouldFindTheClassesReachedThroughApplicationClasses() throws IOException {
    compile(classes, "Card", "class Card {}");
    compile(classes, "Panel", "class Panel { Card card = new Card(); }");
    compile(classes, "View", "class View { Panel panel = new Panel(); }");
    compile(classes, "Unused", "class Unused {}");

    ClassReferenceIndex.Walk walk =
        index.newWalk(loader, Set.of("Card", "Panel", "Unused"), Set.of());

    assertEquals(Set.of("Card", "Panel"), walk.reach("View"));
    assertEquals(Set.of("Unused"), walk.getUnreached());
    assertFalse(walk.isComplete());
  }

  @Test
  void shouldFindTheClassesDeclaredAsFieldsAndParameters() throws IOException {
    compile(classes, "Card", "class Card {}");
    compile(classes, "Panel", "class Panel {}");
    compile(classes, "Badge", "class Badge {}");
    compile(classes, "View",
        "class View { Card card; View(Panel panel) {} Badge[][] badges() { return null; } }");

    ClassReferenceIndex.Walk walk =
        index.newWalk(loader, Set.of("Card", "Panel", "Badge"), Set.of());

    assertEquals(Set.of("Card", "Panel", "Badge"), walk.reach("View"));
    assertTrue(walk.isComplete());
  }

  @Test
  void shouldFindTheClassesDeclaredAsTypeArguments() throws IOException {
    compile(classes, "Card", "class Card {}");
    compile(classes, "Badge", "class Badge {}");
    compile(classes, "Holder", "class Holder<T> { class Inner {} }");
    compile(classes, "View", "import java.util.List; class View { List<Card> cards;"
        + " Holder<String>.Inner inner; List<Badge> badges() { return null; } }");

    ClassReferenceIndex.Walk walk =
        index.newWalk(loader, Set.of("Card", "Badge", "Holder$Inner"), Set.of());

    assertEquals(Set.of("Card", "Badge", "Holder$Inner"), walk.reach("View"));
  }

  @Test
  void shouldStopAtTheBoundaryClasses() throws IOException {
    compile(classes, "Card", "class Card {}");
    compile(classes, "View", "class View { Card card = new Card(); }");
    compile(classes, "Layout", "class Layout { Class<?> target = View.class; }");

    ClassReferenceIndex.Walk walk = index.newWalk(loader, Set.of("View", "Card"), Set.of("View"));

    assertTrue(walk.reach("Layout").isEmpty());
    assertEquals(Set.of("Card"), walk.getUnreached());
  }

  @Test
  void shouldBeCompleteFromTheStartWhenOnlyBoundariesChanged() throws IOException {
    compile(classes, "View", "class View {}");

    ClassReferenceIndex.Walk walk = index.newWalk(loader, Set.of("View"), Set.of("View"));

    assertTrue(walk.isComplete());
    assertTrue(walk.reach("View").isEmpty());
  }

  @Test
  void shouldCountTheOriginAsReached() throws IOException {
    compile(classes, "View", "class View {}");

    assertEquals(Set.of("View"), index.newWalk(loader, Set.of("View"), Set.of()).reach("View"));
  }

  @Test
  void shouldReachEveryClassOnceAcrossTheOrigins() throws IOException {
    compile(classes, "Card", "class Card {}");
    compile(classes, "Shared", "class Shared { Card card = new Card(); }");
    compile(classes, "Layout", "class Layout { Shared shared = new Shared(); }");
    compile(classes, "View", "class View { Shared shared = new Shared(); }");

    ClassReferenceIndex.Walk walk = index.newWalk(loader, Set.of("Card"), Set.of());

    assertEquals(Set.of("Card"), walk.reach("Layout"));
    assertTrue(walk.reach("View").isEmpty());
    assertTrue(walk.isComplete());
  }

  @Test
  void shouldNeverFollowTheReferencesOfLibraryClasses() throws IOException {
    compile(classes, "View", "class View { Object text = new StringBuilder(); }");
    String builder = StringBuilder.class.getName();
    String parent = StringBuilder.class.getSuperclass().getName();

    ClassReferenceIndex.Walk walk = index.newWalk(loader, Set.of(builder, parent), Set.of());

    assertEquals(Set.of(builder), walk.reach("View"));
  }

  @Test
  void shouldRereadTheClassFileThatChangedOnDisk() throws IOException {
    compile(classes, "Card", "class Card {}");
    compile(classes, "View", "class View {}");
    assertTrue(index.newWalk(loader, Set.of("Card"), Set.of()).reach("View").isEmpty());

    Path viewFile = compile(classes, "View", "class View { Card card = new Card(); }");
    Files.setLastModifiedTime(viewFile,
        FileTime.fromMillis(Files.getLastModifiedTime(viewFile).toMillis() + 1000));

    assertEquals(Set.of("Card"), index.newWalk(loader, Set.of("Card"), Set.of()).reach("View"));
  }

  @Test
  void shouldForgetTheChangedClassesWhateverTheFileTimeSays() throws IOException {
    compile(classes, "Card", "class Card {}");
    Path viewFile = compile(classes, "View", "class View {}");
    FileTime firstTime = Files.getLastModifiedTime(viewFile);
    assertTrue(index.newWalk(loader, Set.of("Card"), Set.of()).reach("View").isEmpty());

    compile(classes, "View", "class View { Card card = new Card(); }");
    Files.setLastModifiedTime(viewFile, firstTime);

    assertEquals(Set.of("View", "Card"),
        index.newWalk(loader, Set.of("View", "Card"), Set.of()).reach("View"));
  }

  @Test
  void shouldForgetTheClassFileThatDisappeared() throws IOException {
    compile(classes, "Card", "class Card {}");
    Path viewFile = compile(classes, "View", "class View { Card card = new Card(); }");
    assertEquals(Set.of("Card"), index.newWalk(loader, Set.of("Card"), Set.of()).reach("View"));

    Files.delete(viewFile);

    assertTrue(index.newWalk(loader, Set.of("Card"), Set.of()).reach("View").isEmpty());
  }

  @Test
  void shouldKeepTheEntriesOfEachClassLoaderApart() throws IOException {
    compile(classes, "Card", "class Card {}");
    compile(classes, "View", "class View { Card card = new Card(); }");
    assertEquals(Set.of("Card"), index.newWalk(loader, Set.of("Card"), Set.of()).reach("View"));

    Path otherClasses = classes.resolve("other");
    compile(otherClasses, "View", "class View {}");

    try (URLClassLoader otherLoader = newLoader(otherClasses.toUri().toURL())) {
      assertTrue(index.newWalk(otherLoader, Set.of("Card"), Set.of()).reach("View").isEmpty());
    }
  }

  @Test
  void shouldNotKeepTheClassLoaderAlive() throws IOException, InterruptedException {
    compile(classes, "Card", "class Card {}");
    compile(classes, "View", "class View { Card card = new Card(); }");

    ReferenceQueue<ClassLoader> queue = new ReferenceQueue<>();
    WeakReference<ClassLoader> collected = populateWithDiscardedLoader(queue);

    Reference<? extends ClassLoader> cleared = null;
    for (int attempt = 0; attempt < 50 && cleared == null; attempt++) {
      System.gc();
      cleared = queue.remove(100);
    }

    assertSame(collected, cleared, "the index kept a discarded class loader alive");
  }

  @Test
  void shouldFailForTheUnreadableClassFile() throws IOException {
    Files.write(classes.resolve("View.class"), new byte[] {1, 2, 3, 4});

    ClassReferenceIndex.Walk walk = index.newWalk(loader, Set.of("Card"), Set.of());

    assertThrows(UncheckedIOException.class, () -> walk.reach("View"));
  }

  @Test
  void shouldFailForTheClassFileLocationWithAnAuthority() throws IOException {
    compile(classes, "View", "class View {}");
    URL located = URI.create("file://localhost" + classes.toUri().getRawPath()).toURL();

    try (URLClassLoader remoteLoader = newLoader(located)) {
      ClassReferenceIndex.Walk walk = index.newWalk(remoteLoader, Set.of("Card"), Set.of());

      assertThrows(UncheckedIOException.class, () -> walk.reach("View"));
    }
  }

  private WeakReference<ClassLoader> populateWithDiscardedLoader(ReferenceQueue<ClassLoader> queue)
      throws IOException {
    URLClassLoader discarded = newLoader(classes.toUri().toURL());
    assertEquals(Set.of("Card"), index.newWalk(discarded, Set.of("Card"), Set.of()).reach("View"));
    discarded.close();

    return new WeakReference<>(discarded, queue);
  }

  private static URLClassLoader newLoader(URL location) {
    return new URLClassLoader(new URL[] {location}, ClassLoader.getPlatformClassLoader());
  }

  private Path compile(Path target, String className, String source) throws IOException {
    Path sourceFile = target.resolve("src").resolve(className + ".java");
    Files.createDirectories(sourceFile.getParent());
    Files.writeString(sourceFile, source);

    JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
    int status = compiler.run(null, null, null, "-d", target.toString(), "-cp", target.toString(),
        sourceFile.toString());
    assertEquals(0, status, "compiling " + className);

    return target.resolve(className + ".class");
  }
}
