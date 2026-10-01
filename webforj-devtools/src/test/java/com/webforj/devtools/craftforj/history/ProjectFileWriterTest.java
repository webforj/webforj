package com.webforj.devtools.craftforj.history;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mockStatic;

import com.webforj.devtools.craftforj.history.model.HistoryEntryInfo;
import com.webforj.devtools.craftforj.history.model.HistoryFileSnapshot;
import com.webforj.devtools.craftforj.source.staging.SourceHasher;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.CopyOption;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.mockito.MockedStatic;

@DisplayName("ProjectFileWriter")
class ProjectFileWriterTest {

  @TempDir
  Path project;

  @Nested
  @DisplayName("write")
  class Write {

    @Test
    @DisplayName("Should write the content over the file as UTF-8")
    void shouldWriteContent() throws IOException {
      Path file = project.resolve("View.java");
      Files.writeString(file, "class View {}\n");

      ProjectFileWriter.write(file, "class View { String s = \"größe\"; }\n");

      assertEquals("class View { String s = \"größe\"; }\n", Files.readString(file));
    }
  }

  @Nested
  @DisplayName("writeAtomic")
  class WriteAtomic {

    @Test
    @DisplayName("Should replace the file in one move and create its folders")
    void shouldReplaceFile() throws IOException {
      Path file = project.resolve("src/main/frontend/app.css");

      ProjectFileWriter.writeAtomic(file, "a {}\n");
      ProjectFileWriter.writeAtomic(file, "b {}\n");

      assertEquals("b {}\n", Files.readString(file));
      try (Stream<Path> files = Files.list(file.getParent())) {
        assertEquals(List.of(file), files.toList());
      }
    }

    @ParameterizedTest
    @MethodSource("getModes")
    @DisplayName("Should keep the mode of the file it replaces")
    void shouldKeepModeOnReplace(String mode) throws IOException {
      assumeTrue(isPosix());
      Path file = project.resolve("app.css");
      Files.writeString(file, "a {}\n");
      Files.setPosixFilePermissions(file, PosixFilePermissions.fromString(mode));

      ProjectFileWriter.writeAtomic(file, "b {}\n");

      assertEquals("b {}\n", Files.readString(file));
      assertEquals(mode, PosixFilePermissions.toString(Files.getPosixFilePermissions(file)));
    }

    @Test
    @DisplayName("Should create a new file with the default mode")
    void shouldCreateNewFileWithDefaultMode() throws IOException {
      assumeTrue(isPosix());
      Path probe = project.resolve("probe.css");
      Files.writeString(probe, "probe\n");
      Set<PosixFilePermission> defaults = Files.getPosixFilePermissions(probe);
      Path file = project.resolve("app.css");

      ProjectFileWriter.writeAtomic(file, "a {}\n");

      assertEquals("a {}\n", Files.readString(file));
      assertEquals(defaults, Files.getPosixFilePermissions(file));
    }

    @Test
    @DisplayName("Should keep the mode when the file system cannot move atomically")
    void shouldKeepModeWithoutAtomicMove() throws IOException {
      assumeTrue(isPosix());
      Path file = project.resolve("app.css");
      Files.writeString(file, "a {}\n");
      Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rw-rw-r--"));

      try (MockedStatic<Files> files = mockStatic(Files.class, CALLS_REAL_METHODS)) {
        files
            .when(() -> Files.move(any(Path.class), eq(file), any(CopyOption.class),
                eq(StandardCopyOption.ATOMIC_MOVE)))
            .thenThrow(new AtomicMoveNotSupportedException(null, null, "no atomic move"));

        ProjectFileWriter.writeAtomic(file, "b {}\n");

        files.verify(
            () -> Files.move(any(Path.class), eq(file), eq(StandardCopyOption.REPLACE_EXISTING)));
      }

      assertEquals("b {}\n", Files.readString(file));
      assertEquals("rw-rw-r--", PosixFilePermissions.toString(Files.getPosixFilePermissions(file)));
      try (Stream<Path> listed = Files.list(project)) {
        assertEquals(List.of(file), listed.toList());
      }
    }

    private static Stream<String> getModes() {
      return Stream.of("rw-rw-r--", "rw-r-----");
    }
  }

  @Nested
  @DisplayName("reset")
  class Reset {

    @Test
    @DisplayName("Should restore bytes over a file keeping its mode, and record no step")
    void shouldRestoreWithoutStep() throws IOException {
      assumeTrue(isPosix());
      Path file = project.resolve("app.css");
      Files.writeString(file, "a {}\n");
      Files.setPosixFilePermissions(file, PosixFilePermissions.fromString("rw-rw-r--"));
      HistoryJournal journal = new HistoryJournal(project.resolve("journal"));
      final HistoryStep step = journal.openStep();
      byte[] content = "\uFEFFb {}\r\n".getBytes(StandardCharsets.UTF_8);

      ProjectFileWriter.reset(file, content, null);

      assertArrayEquals(content, Files.readAllBytes(file));
      assertEquals("rw-rw-r--", PosixFilePermissions.toString(Files.getPosixFilePermissions(file)));
      assertEquals(null, step.close());
      try (Stream<Path> listed = Files.list(project)) {
        assertEquals(List.of(file), listed.toList());
      }
    }

    @Test
    @DisplayName("Should restore a missing file with the mode it is given and delete for no "
        + "content")
    void shouldRestoreMissingFileWithMode() throws IOException {
      assumeTrue(isPosix());
      Path file = project.resolve("src/New.java");

      ProjectFileWriter.reset(file, "class New {}\n".getBytes(),
          PosixFilePermissions.fromString("rwxr-x---"));

      assertEquals("class New {}\n", Files.readString(file));
      assertEquals("rwxr-x---", PosixFilePermissions.toString(Files.getPosixFilePermissions(file)));

      ProjectFileWriter.reset(file, null, null);

      assertFalse(Files.exists(file));
    }

    @Test
    @DisplayName("Should restore through a symbolic link and keep it a link")
    void shouldRestoreThroughLink() throws IOException {
      Path real = project.resolve("real/View.java");
      Files.createDirectories(real.getParent());
      Files.writeString(real, "one\n");
      Path link = project.resolve("Linked.java");
      try {
        Files.createSymbolicLink(link, real);
      } catch (IOException | UnsupportedOperationException e) {
        assumeTrue(false, "The file system cannot create links");
      }

      ProjectFileWriter.reset(link, "two\n".getBytes(), null);

      assertTrue(Files.isSymbolicLink(link));
      assertEquals("two\n", Files.readString(real));
    }

    @Test
    @DisplayName("Should restore a file on a store without POSIX permissions")
    void shouldRestoreOnStoreWithoutPosix() throws IOException {
      try (FileSystem zip =
          FileSystems.newFileSystem(project.resolve("store.zip"), Map.of("create", "true"))) {
        Path file = zip.getPath("Source.java");
        Files.writeString(file, "one\n");

        ProjectFileWriter.reset(file, "two\n".getBytes(), null);
        ProjectFileWriter.reset(zip.getPath("New.java"), "new\n".getBytes(),
            PosixFilePermissions.fromString("rw-r-----"));

        assertEquals("two\n", Files.readString(file));
        assertEquals("new\n", Files.readString(zip.getPath("New.java")));
      }
    }
  }

  @Nested
  @DisplayName("remove")
  class Remove {

    @Test
    @DisplayName("Should delete the file and answer whether it existed")
    void shouldDeleteFile() throws IOException {
      Path file = project.resolve("View.java");
      Files.writeString(file, "class View {}\n");

      assertTrue(ProjectFileWriter.remove(file));
      assertFalse(Files.exists(file));
      assertFalse(ProjectFileWriter.remove(file));
    }
  }

  @Nested
  @DisplayName("step")
  class Step {

    @Test
    @DisplayName("Should give the open step each file before it changes, and no other thread's "
        + "file")
    void shouldGiveStepFilesBeforeChange() throws Exception {
      Path file = project.resolve("View.java");
      Path sheet = project.resolve("app.css");
      final Path other = project.resolve("Other.java");
      Files.writeString(file, "one\n");
      HistoryJournal journal = new HistoryJournal(project.resolve("journal"));
      final HistoryStep step = journal.openStep();

      ProjectFileWriter.write(file, "two\n");
      ProjectFileWriter.writeAtomic(sheet, "a {}\n");
      Thread thread = new Thread(() -> {
        try {
          ProjectFileWriter.write(other, "other\n");
        } catch (IOException e) {
          throw new IllegalStateException(e);
        }
      });
      thread.start();
      thread.join();
      ProjectFileWriter.remove(file);
      Long id = step.close();

      HistoryEntryInfo entry = journal.getInfo().getEntries().get(0);
      assertEquals(id, entry.getId());
      assertEquals(Stream.of(sheet.toString(), file.toString()).sorted().toList(),
          entry.getFiles().stream().map(HistoryFileSnapshot::getPath).sorted().toList());
      assertEquals(SourceHasher.hash("one\n".getBytes(StandardCharsets.UTF_8)),
          entry.getFiles().stream().filter(snapshot -> snapshot.getPath().equals(file.toString()))
              .findFirst().orElseThrow().getBefore());
    }

    @Test
    @DisplayName("Should record nothing once the step is closed")
    void shouldRecordNothingOnceClosed() throws IOException {
      Path file = project.resolve("View.java");
      HistoryJournal journal = new HistoryJournal(project.resolve("journal"));
      journal.openStep().close();

      ProjectFileWriter.write(file, "cleared\n");

      assertTrue(journal.getInfo().getEntries().isEmpty());
      assertEquals("cleared\n", Files.readString(file));
    }
  }

  private static boolean isPosix() {
    return FileSystems.getDefault().supportedFileAttributeViews().contains("posix");
  }
}
