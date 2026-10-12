package com.webforj.plugin.foundation.bbj;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class FileTreeTest {

  @TempDir
  Path temporary;

  @Test
  void shouldRemoveTheDirectoryWithEveryFileInside() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));

    FileTree.removeDirectory(app);

    assertFalse(Files.exists(app));
  }

  @Test
  void shouldIgnoreDirectoryThatDoesNotExist() {
    Path missing = temporary.resolve("missing");

    assertDoesNotThrow(() -> FileTree.removeDirectory(missing));
  }

  @Test
  void shouldRemoveSymbolicLinkWithoutTheFilesItPointsTo() throws IOException {
    Path app = HelloApp.createCopy(temporary.resolve("hello-app"));
    Path folder = Files.createDirectory(temporary.resolve("folder"));
    try {
      Files.createSymbolicLink(folder.resolve("hello-app"), app);
    } catch (UnsupportedOperationException unsupported) {
      assumeTrue(false, "The file system does not support symbolic links");
    }

    FileTree.removeDirectory(folder);

    assertFalse(Files.exists(folder, LinkOption.NOFOLLOW_LINKS));
    assertTrue(Files.isRegularFile(app.resolve("src/main/resources/webforj.conf")));
  }

  @Test
  void shouldCreateDirectoryAndFileOnlyTheOwnerCanRead() throws IOException {
    assumeTrue(Files.getFileStore(temporary).supportsFileAttributeView("posix"),
        "The file system has no owner permissions");

    Path directory = FileTree.createPrivateDirectory(temporary);
    final Path file = FileTree.createPrivateFile(directory, "program", ".log");

    assertEquals(temporary, directory.getParent());
    assertTrue(directory.getFileName().toString().startsWith(".webforj-bbj-setup-"));
    assertEquals(PosixFilePermissions.fromString("rwx------"),
        Files.getPosixFilePermissions(directory));
    assertEquals(directory, file.getParent());
    assertTrue(file.getFileName().toString().startsWith("program"));
    assertEquals(PosixFilePermissions.fromString("rw-------"), Files.getPosixFilePermissions(file));
  }
}
