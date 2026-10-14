package com.webforj.bbjship.foundation;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystemNotFoundException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Stream;

final class FixtureFolder {

  private FixtureFolder() {}

  static synchronized Path getFolder(String name) throws IOException {
    String folder = "fixtures/" + name;
    URI fixture = getResource(folder);
    try {
      return Path.of(fixture);
    } catch (FileSystemNotFoundException closed) {
      FileSystem archive = FileSystems.newFileSystem(fixture, Map.of()); // NOSONAR

      return archive.getPath(folder);
    }
  }

  static Path createCopy(Path source, Path target) throws IOException {
    try (Stream<Path> paths = Files.walk(source)) {
      for (Path file : paths.toList()) {
        Path copy = target.resolve(source.relativize(file).toString());
        if (Files.isDirectory(file)) {
          Files.createDirectories(copy);
        } else {
          Files.copy(file, copy);
        }
      }
    }

    return target;
  }

  private static URI getResource(String folder) throws IOException {
    try {
      return FixtureFolder.class.getClassLoader().getResource(folder).toURI();
    } catch (URISyntaxException invalid) {
      throw new IOException(invalid);
    }
  }
}
