package com.webforj.bbjship.foundation;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * The bbjship-plugin fixture, the files of the BBjShip plugin ZIP with a launcher that records its
 * calls.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class BBjShipPackage {

  private static final String JAR_FOLDER = ".jar.d";

  private BBjShipPackage() {}

  /**
   * Copies the plugin files of the fixture into a folder.
   *
   * @param target the folder to copy into
   *
   * @return the folder holding the copy
   * @throws IOException if the fixture cannot be read or the copy cannot be written
   */
  public static Path createCopy(Path target) throws IOException {
    return FixtureFolder
        .createCopy(FixtureFolder.getFolder("bbjship-plugin").resolve("webforj-bbjship"), target);
  }

  /**
   * Writes the plugin ZIP of a copy, packing each folder named after a jar into that jar.
   *
   * @param plugin the folder holding the copy
   * @param zip the ZIP to write
   *
   * @return the written ZIP
   * @throws IOException if the copy cannot be read or the ZIP cannot be written
   */
  public static Path writeZip(Path plugin, Path zip) throws IOException {
    try (ZipOutputStream output = new ZipOutputStream(Files.newOutputStream(zip));
        Stream<Path> paths = Files.walk(plugin)) {
      for (Path path : paths.sorted().toList()) {
        String name = toEntryName(plugin, path);
        if (path.equals(plugin) || name.contains(JAR_FOLDER + "/")) {
          continue;
        }

        if (name.endsWith(JAR_FOLDER)) {
          output.putNextEntry(new ZipEntry(name.substring(0, name.length() - 2)));
          writeJar(path, output);
        } else if (Files.isDirectory(path)) {
          output.putNextEntry(new ZipEntry(name + "/"));
        } else {
          output.putNextEntry(new ZipEntry(name));
          Files.copy(path, output);
        }

        output.closeEntry();
      }
    }

    return zip;
  }

  private static void writeJar(Path folder, ZipOutputStream zip) throws IOException {
    Path manifestFile = folder.resolve(JarFile.MANIFEST_NAME);
    JarOutputStream jar;
    if (Files.isRegularFile(manifestFile)) {
      try (InputStream input = Files.newInputStream(manifestFile)) {
        jar = new JarOutputStream(zip, new Manifest(input));
      }
    } else {
      jar = new JarOutputStream(zip);
    }

    try (Stream<Path> paths = Files.walk(folder)) {
      for (Path file : paths.filter(Files::isRegularFile).sorted().toList()) {
        String name = toEntryName(folder, file);
        if (!name.equals(JarFile.MANIFEST_NAME)) {
          jar.putNextEntry(new ZipEntry(name));
          Files.copy(file, jar);
          jar.closeEntry();
        }
      }
    }

    jar.finish();
  }

  private static String toEntryName(Path root, Path path) {
    return root.relativize(path).toString().replace(File.separatorChar, '/');
  }
}
