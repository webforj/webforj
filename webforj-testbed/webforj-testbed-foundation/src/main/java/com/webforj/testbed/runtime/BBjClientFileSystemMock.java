package com.webforj.testbed.runtime;

import com.basis.bbj.proxies.BBjClientFile;
import com.basis.startup.type.BBjException;
import com.webforj.testbed.DownloadedFile;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/**
 * The mock backing the file system of the client, holding the files of one test context.
 *
 * <p>
 * Driven by {@code com.webforj.UploadedFile} and {@code com.webforj.Page}, which look files up by
 * the name they carry on the client.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class BBjClientFileSystemMock extends RuntimeMock {
  private static final String CACHE_PREFIX = "testbed-clientfs-";

  private final Map<String, BBjClientFileMock> files = new HashMap<>();
  private final List<DownloadedFile> downloads = new ArrayList<>();
  private Path cacheDirectory;

  /**
   * Stores a file on the client.
   *
   * @param name the name of the file on the client
   * @param contents the bytes of the file
   */
  void put(String name, byte[] contents) {
    getFile(name).setContents(contents);
  }

  /**
   * Returns the file with the given name, which holds no bytes until some are stored.
   *
   * @param name the name of the file on the client
   * @return the file
   */
  BBjClientFileMock getFile(String name) {
    return files.computeIfAbsent(name, key -> new BBjClientFileMock(this, key));
  }

  /**
   * Records that the browser was asked to download the given bytes under the given name.
   *
   * @param name the name of the file on the client
   * @param contents the bytes of the file
   */
  void addDownload(String name, byte[] contents) {
    downloads.add(new DownloadedFile(name, contents));
  }

  /**
   * Returns the files the browser was asked to download, in the order it was asked.
   *
   * @return the files
   */
  List<DownloadedFile> getDownloads() {
    return List.copyOf(downloads);
  }

  /**
   * Returns the directory holding the server copies the application named no destination for.
   *
   * @return the directory, created on first use
   * @throws BBjException if the directory cannot be created
   */
  Path getCacheDirectory() throws BBjException {
    if (cacheDirectory == null) {
      try {
        cacheDirectory = createOwnerOnlyDirectory();
      } catch (IOException e) {
        throw new BBjException(e, BBjException.IO_ERR);
      }
    }

    return cacheDirectory;
  }

  /**
   * Forgets every file and removes the server copies this file system owns.
   */
  void dispose() {
    files.values().forEach(BBjClientFileMock::clear);
    files.clear();
    downloads.clear();
    if (cacheDirectory == null) {
      return;
    }

    try (Stream<Path> paths = Files.walk(cacheDirectory)) {
      for (Path path : paths.sorted(Comparator.reverseOrder()).toList()) {
        Files.deleteIfExists(path);
      }
    } catch (IOException e) {
      throw new UncheckedIOException("Failed to remove the client file cache", e);
    } finally {
      cacheDirectory = null;
    }
  }

  @Override
  protected Object handle(Object proxy, Method method, Object[] args) throws Exception {
    return switch (method.getName()) {
      // The runtime resolves a file by its name alone, whatever parent the caller names.
      case "getClientFile" -> getFile((String) args[args.length - 1]).getProxy(BBjClientFile.class);
      default -> UNHANDLED;
    };
  }

  private static Path createOwnerOnlyDirectory() throws IOException {
    try {
      return Files.createTempDirectory(CACHE_PREFIX,
          PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rwx------")));
    } catch (UnsupportedOperationException e) {
      return Files.createTempDirectory(CACHE_PREFIX); // NOSONAR
    }
  }
}
