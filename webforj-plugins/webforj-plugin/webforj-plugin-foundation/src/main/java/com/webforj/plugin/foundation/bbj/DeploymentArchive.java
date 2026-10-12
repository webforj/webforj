package com.webforj.plugin.foundation.bbj;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

/**
 * Reads and writes BBj deployment archives.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class DeploymentArchive {

  /**
   * The largest number of bytes an archive may hold, compressed or extracted.
   */
  public static final long MAX_BYTES = 1024L * 1024 * 1024;

  /**
   * The message of an archive that exceeds {@link #MAX_BYTES}.
   */
  public static final String TOO_LARGE_MESSAGE =
      "BBj deployment archive exceeds the allowed size of 1 GiB";

  /**
   * The name of the archive entry that marks a development archive and names its class folders.
   */
  public static final String DEVELOPMENT_PROPERTIES = "development.properties";

  /**
   * The key of the development properties that names the class folders of a development run.
   *
   * <p>
   * The value lists the absolute class folders in classpath order, separated by the path separator
   * of the BBj machine.
   * </p>
   */
  public static final String CLASS_FOLDER_KEY = "classFolder";

  /**
   * The name of the file in the BBjShip runtime folder that enables development runs.
   */
  public static final String DEVELOPMENT_SWITCH = "development.enabled";

  /**
   * The name of the folder beside the BBjShip plugin folder that holds every file BBjShip writes at
   * runtime.
   */
  public static final String RUNTIME_DIRECTORY = ".webforj-bbjship-deployments";

  private static final int MAX_ENTRIES = 10000;
  private static final Set<String> RUNTIME_ENTRIES =
      Set.of("deployments", "htdocs", "assets", DEVELOPMENT_SWITCH, RUNTIME_DIRECTORY);

  private DeploymentArchive() {}

  /**
   * Writes application files into an archive.
   *
   * @param archive the output archive
   * @param files the files keyed by their archive names
   *
   * @throws IOException if the files are invalid or the archive cannot be written
   */
  static void write(Path archive, Map<String, Path> files) throws IOException {
    if (files.size() >= MAX_ENTRIES) {
      throw new ZipException("BBj deployment archive contains too many files");
    }

    Path output = archive.toAbsolutePath().normalize();
    for (Map.Entry<String, Path> file : files.entrySet()) {
      applyNameValidation(file.getKey());
      if (!Files.isRegularFile(file.getValue())) {
        throw new IOException("BBj deployment payload file is missing: " + file.getValue());
      }

      if (Files.exists(output) && Files.isSameFile(output, file.getValue())) {
        throw new IOException(
            "BBj deployment archive must not replace a payload source. Choose another output path");
      }
    }

    Path temporary = Files.createTempFile(output.getParent(), ".bbj-deployment-", ".zip");
    try {
      try (ZipOutputStream zip = new ZipOutputStream(Files.newOutputStream(temporary))) {
        long remaining = MAX_BYTES;
        for (Map.Entry<String, Path> file : files.entrySet()) {
          zip.putNextEntry(new ZipEntry(file.getKey()));
          try (InputStream input = Files.newInputStream(file.getValue())) {
            remaining -= writeContent(input, zip, remaining);
          }

          zip.closeEntry();
        }
      }

      Files.move(temporary, output, StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING);
    } finally {
      Files.deleteIfExists(temporary);
    }
  }

  /**
   * Tells whether an archive is a development archive, one that names class folders in place of an
   * application jar.
   *
   * @param archive the archive
   *
   * @return true when the archive carries the development properties
   * @throws IOException if the archive cannot be read
   */
  public static boolean isDevelopment(Path archive) throws IOException {
    try (ZipFile zip = new ZipFile(archive.toFile())) {
      return zip.getEntry(DEVELOPMENT_PROPERTIES) != null;
    }
  }

  /**
   * Extracts a deployment archive into a new directory.
   *
   * @param archive the uploaded archive
   * @param directory the new deployment directory
   *
   * @throws ArchiveSizeException if the archive unpacks to more than {@link #MAX_BYTES}
   * @throws ZipException if the archive holds an entry outside the deployment layout or too many
   *         entries
   * @throws IOException if the archive cannot be read or extracted
   */
  public static void read(Path archive, Path directory) throws IOException {
    read(archive, directory, MAX_BYTES);
  }

  static void read(Path archive, Path directory, long maxBytes) throws IOException {
    readArchive(archive, directory, false, maxBytes);
  }

  static void readPlugin(Path archive, Path directory) throws IOException {
    readArchive(archive, directory, true, MAX_BYTES);
  }

  private static void readArchive(Path archive, Path directory, boolean plugin, long maxBytes)
      throws IOException {
    Path destination = directory.toAbsolutePath().normalize();
    Files.createDirectory(destination);
    try {
      writeEntries(archive, destination, plugin, maxBytes);
    } catch (IOException | RuntimeException failure) {
      try {
        FileTree.removeDirectory(destination);
      } catch (IOException cleanup) {
        failure.addSuppressed(cleanup);
      }

      throw failure;
    }
  }

  private static void writeEntries(Path archive, Path destination, boolean plugin, long maxBytes)
      throws IOException {
    Set<String> names = new HashSet<>();
    long remaining = maxBytes;
    try (ZipInputStream zip = new ZipInputStream(Files.newInputStream(archive))) {
      ZipEntry entry = zip.getNextEntry();
      if (entry == null && !plugin) {
        throw new ZipException("The uploaded body is not a BBj deployment archive. Deploy the "
            + "ZIP archive the build writes");
      }

      for (; entry != null; entry = zip.getNextEntry()) {
        String name = entry.getName();
        applyEntryValidation(name, plugin);

        if (!names.add(name) || names.size() > MAX_ENTRIES) {
          throw new ZipException("BBj deployment archive has duplicate or excessive entries");
        }

        Path file = destination.resolve(name);
        if (entry.isDirectory()) {
          Files.createDirectories(file);
        } else {
          Files.createDirectories(file.getParent());
          try (OutputStream output = Files.newOutputStream(file)) {
            remaining -= writeContent(zip, output, remaining);
          }
        }

        zip.closeEntry();
      }
    }
  }

  private static void applyEntryValidation(String name, boolean plugin) throws IOException {
    if (plugin) {
      applyPluginNameValidation(name);
      return;
    }

    applyNameValidation(name);
  }

  private static void applyNameValidation(String name) throws IOException {
    boolean allowed = "application.jar".equals(name) || DEVELOPMENT_PROPERTIES.equals(name)
        || name.startsWith("dependencies/") || name.toLowerCase(Locale.ROOT).endsWith(".bbj");
    if (!allowed || name.contains("\\") || name.contains(":") || name.startsWith("/")) {
      throw new ZipException("Invalid BBj deployment archive entry: " + name);
    }

    for (String part : name.split("/")) {
      if (part.isEmpty() || ".".equals(part) || "..".equals(part)) {
        throw new ZipException("Invalid BBj deployment archive entry: " + name);
      }
    }
  }

  private static void applyPluginNameValidation(String name) throws IOException {
    String entry = name.endsWith("/") ? name.substring(0, name.length() - 1) : name;
    if (entry.isEmpty() || entry.contains("\\") || entry.contains(":") || entry.startsWith("/")) {
      throw new ZipException("Invalid BBj plugin archive entry: " + name);
    }

    for (String part : entry.split("/", -1)) {
      if (part.isEmpty() || ".".equals(part) || "..".equals(part)) {
        throw new ZipException("Invalid BBj plugin archive entry: " + name);
      }
    }

    if (RUNTIME_ENTRIES.contains(entry.split("/")[0])) {
      throw new ZipException("BBj plugin archive must not contain runtime files");
    }
  }

  private static long writeContent(InputStream input, OutputStream output, long limit)
      throws IOException {
    byte[] buffer = new byte[8192];
    long total = 0;
    int size;
    while ((size = input.read(buffer)) != -1) {
      total += size;
      if (total > limit) {
        throw new ArchiveSizeException();
      }

      output.write(buffer, 0, size);
    }

    return total;
  }

  /**
   * Reports an archive whose content exceeds {@link #MAX_BYTES}.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  public static final class ArchiveSizeException extends ZipException {

    private static final long serialVersionUID = 1L;

    private ArchiveSizeException() {
      super(TOO_LARGE_MESSAGE);
    }
  }
}
