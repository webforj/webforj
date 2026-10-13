package com.webforj.bbjship.cli.command;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Stream;

/**
 * Copies the BBj home fixture into a folder.
 */
public final class Home {

  /**
   * Holds the plugin folder relative to the BBj home.
   */
  public static final String PLUGIN_FOLDER = "plugins/webforj-bbjship";

  /**
   * Holds the deployments folder relative to the BBj home.
   */
  public static final String DEPLOYMENTS_FOLDER = "plugins/.webforj-bbjship-deployments";

  /**
   * Holds the document base of the BBjShip context relative to the BBj home.
   */
  public static final String HTDOCS_FOLDER = "plugins/.webforj-bbjship-deployments/htdocs";

  /**
   * Holds the document base of the assets context relative to the BBj home.
   */
  public static final String ASSETS_FOLDER = "plugins/.webforj-bbjship-deployments/assets";

  private static final String JAR_FOLDER = ".jar.d";

  private Home() {}

  /**
   * Copies the BBj home fixture into the target folder and returns the real path of the copy.
   */
  public static Path createCopy(Path target) throws IOException, URISyntaxException {
    Path source = Path.of(Home.class.getClassLoader().getResource("fixtures/bbj-home").toURI());
    List<Path> paths;
    try (Stream<Path> files = Files.walk(source)) {
      paths = files.sorted().toList();
    }

    for (Path path : paths) {
      String relative = source.relativize(path).toString();
      if (relative.contains(JAR_FOLDER)) {
        if (relative.endsWith(JAR_FOLDER)) {
          writeJar(path, target.resolve(relative.substring(0, relative.length() - 2)));
        }

        continue;
      }

      if (Files.isDirectory(path)) {
        Files.createDirectories(target.resolve(relative));
      } else {
        Files.copy(path, target.resolve(relative));
      }
    }

    return target.toRealPath();
  }

  private static void writeJar(Path folder, Path jar) throws IOException {
    Manifest manifest;
    try (InputStream input = Files.newInputStream(folder.resolve("META-INF/MANIFEST.MF"))) {
      manifest = new Manifest(input);
    }

    try (OutputStream file = Files.newOutputStream(jar);
        JarOutputStream output = new JarOutputStream(file, manifest)) {
      output.finish();
    }
  }
}
