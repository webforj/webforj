package com.webforj.testbed.runtime;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.BBjClientFile;
import com.basis.bbj.proxies.BBjClientFileSystem;
import com.basis.startup.type.BBjException;
import com.webforj.Environment;
import com.webforj.testbed.WebforjTestbed;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class BBjClientFileMockTest {
  @TempDir
  Path directory;

  @Test
  void shouldPreserveTheClientNameWithoutSanitizingIt() throws Exception {
    BBjClientFile file = new BBjClientFileMock(new BBjClientFileSystemMock(), "résumé 2026.txt")
        .getProxy(BBjClientFile.class);
    assertEquals("résumé 2026.txt", file.getName());
    assertEquals("résumé 2026.txt", file.getPath());
  }

  @Test
  void shouldReportNativeErrorsForMissingFilesAndExistingDestinations() throws Exception {
    BBjClientFile file = new BBjClientFileMock(new BBjClientFileSystemMock(), "missing.txt")
        .getProxy(BBjClientFile.class);
    BBjException readFailure = assertThrows(BBjException.class, file::getContents);
    assertEquals(BBjException.DOM_FILE, readFailure.getErrorNumber());
    BBjException copyFailure = assertThrows(BBjException.class, file::copyFromClient);
    assertEquals(BBjException.DOM_FILE, copyFailure.getErrorNumber());

    BBjClientFileMock mock = new BBjClientFileMock(new BBjClientFileSystemMock(), "notes.txt");
    mock.setContents(new byte[] {1, 2, 3});
    BBjClientFile existing = mock.getProxy(BBjClientFile.class);
    Path target = directory.resolve("existing.txt");
    Files.write(target, new byte[] {9});
    String targetName = target.toString();
    BBjException destinationFailure =
        assertThrows(BBjException.class, () -> existing.copyFromClient(targetName));
    assertEquals(BBjException.ACCESS, destinationFailure.getErrorNumber());
    assertArrayEquals(new byte[] {9}, Files.readAllBytes(target));
  }

  @Test
  void shouldDistinguishDeletedFilesFromNamesNeverCached() throws Exception {
    BBjClientFileMock mock = new BBjClientFileMock(new BBjClientFileSystemMock(), "notes.txt");
    mock.setContents(new byte[] {1});
    BBjClientFile file = mock.getProxy(BBjClientFile.class);
    assertTrue(file.delete());
    Path target = directory.resolve("deleted-copy.txt");
    String targetName = target.toString();

    BBjException copyFailure =
        assertThrows(BBjException.class, () -> file.copyFromClient(targetName));

    assertEquals(BBjException.IO_ERR, copyFailure.getErrorNumber());
    assertTrue(Files.exists(target));
    assertEquals(0, Files.size(target));
    BBjException readFailure = assertThrows(BBjException.class, file::getContents);
    assertEquals(BBjException.DOM_FILE, readFailure.getErrorNumber());
  }

  @Test
  void shouldHandTheServerFileToTheClientForDownload() throws Exception {
    BBjClientFileSystemMock mock = new BBjClientFileSystemMock();
    BBjClientFile file = mock.getProxy(BBjClientFileSystem.class).getClientFile("report.csv");
    Path source = directory.resolve("source.csv");
    Files.write(source, new byte[] {4, 5, 6});

    file.copyToClient(source.toString());

    assertTrue(file.exists());
    assertEquals(3, file.size());
    assertArrayEquals(new byte[] {4, 5, 6}, file.getContents());
    assertEquals(1, mock.getDownloads().size());
    assertEquals("report.csv", mock.getDownloads().get(0).getName());
    assertArrayEquals(new byte[] {4, 5, 6}, mock.getDownloads().get(0).getContents());
    assertArrayEquals(new byte[] {4, 5, 6}, Files.readAllBytes(source));
  }

  @Test
  void shouldRefuseToDownloadTheMissingServerFile() throws Exception {
    BBjClientFileSystemMock mock = new BBjClientFileSystemMock();
    BBjClientFile file = mock.getProxy(BBjClientFileSystem.class).getClientFile("report.csv");
    String source = directory.resolve("missing.csv").toString();

    BBjException failure = assertThrows(BBjException.class, () -> file.copyToClient(source));

    assertEquals(BBjException.DOM_FILE, failure.getErrorNumber());
    assertFalse(file.exists());
    assertEquals(List.of(), mock.getDownloads());
  }

  @Test
  void shouldDownloadTheContentsWrittenToTheClientFile() throws Exception {
    BBjClientFileSystemMock mock = new BBjClientFileSystemMock();
    BBjClientFile file = mock.getProxy(BBjClientFileSystem.class).getClientFile("notes.txt");

    file.setContents(new byte[] {7});

    assertArrayEquals(new byte[] {7}, file.getContents());
    assertEquals("notes.txt", mock.getDownloads().get(0).getName());
  }

  @Test
  void shouldForgetFilesAndDownloadsOnDisposal() throws Exception {
    BBjClientFileSystemMock mock = new BBjClientFileSystemMock();
    BBjClientFile file = mock.getProxy(BBjClientFileSystem.class).getClientFile("notes.txt");
    file.setContents(new byte[] {7});

    mock.dispose();

    assertEquals(List.of(), mock.getDownloads());
    assertFalse(mock.getProxy(BBjClientFileSystem.class).getClientFile("notes.txt").exists());
  }

  @Test
  void shouldResolveClientNamesIndependentlyOfTheParent() throws BBjException {
    BBjClientFileSystemMock mock = new BBjClientFileSystemMock();
    mock.put("notes.txt", new byte[] {1, 2, 3});
    BBjClientFileSystem files = mock.getProxy(BBjClientFileSystem.class);
    BBjClientFile parent = files.getClientFile("parent");

    BBjClientFile file = files.getClientFile(parent, "notes.txt");

    assertEquals("notes.txt", file.getPath());
    assertArrayEquals(new byte[] {1, 2, 3}, file.getContents());
  }

  @Test
  void shouldRemoveOnlyContextOwnedServerCopiesDuringTeardown() throws Exception {
    WebforjTestbed.setup();
    Path first = null;
    Path second = null;
    try {
      TestbedRuntime.getCurrent().putUploadedFile("notes.txt", new byte[] {1, 2, 3});
      BBjClientFile file = Environment.getCurrent().getBBjAPI().getThinClient()
          .getClientFileSystem().getClientFile("notes.txt");
      first = Path.of(file.copyFromClient());
      second = Path.of(file.copyFromClient());
      assertNotEquals(first, second);
      assertArrayEquals(new byte[] {1, 2, 3}, Files.readAllBytes(first));
      assertArrayEquals(new byte[] {1, 2, 3}, Files.readAllBytes(second));

      Path applicationCopy = Path.of(file.copyFromClient(directory.toString()));
      WebforjTestbed.tearDown();

      assertTrue(Files.exists(applicationCopy));
      assertArrayEquals(new byte[] {1, 2, 3}, Files.readAllBytes(applicationCopy));
      assertFalse(Files.exists(first));
      assertFalse(Files.exists(second));
      assertFalse(Files.exists(first.getParent()));
    } finally {
      WebforjTestbed.tearDown();
      if (first != null) {
        Files.deleteIfExists(first);
      }
      if (second != null) {
        Files.deleteIfExists(second);
      }
    }
  }
}
