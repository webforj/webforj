package com.webforj.bbjship.foundation;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.Set;
import java.util.stream.Collectors;
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
   * The name of the archive entry of the BBj program that starts the application.
   */
  public static final String LAUNCHER_ENTRY = "bbj/webforj.bbj";

  /**
   * The name of the archive entry of the packaged application.
   */
  public static final String APPLICATION_ENTRY = "application.jar";

  /**
   * The name of the archive folder of the dependency jars.
   */
  public static final String DEPENDENCIES_FOLDER = "dependencies";

  private static final String CLASS_FOLDER_KEY = "classFolder";
  private static final int MAX_ENTRIES = 10000;

  private DeploymentArchive() {}

  /**
   * Resolves the deployment archive of a publish name, {@code bbj/<publishName>.zip} in the build
   * directory.
   *
   * @param buildDirectory the build directory of the project
   * @param publishName the publish name the archive is named after
   *
   * @return the archive
   * @throws IllegalArgumentException if the publish name is blank or invalid
   */
  public static Path resolveArchive(Path buildDirectory, String publishName) {
    Optional<String> refusal = PublishNamePolicy.findRefusal(publishName);
    if (refusal.isPresent()) {
      throw new IllegalArgumentException(refusal.get());
    }

    return buildDirectory.resolve("bbj").resolve(publishName + ".zip");
  }

  /**
   * Saves an uploaded archive to a file.
   *
   * @param content the uploaded archive
   * @param archive the file the archive is saved to
   *
   * @throws ArchiveSizeException if the upload holds more than {@link #MAX_BYTES}
   * @throws IOException if the upload cannot be read or the file cannot be written
   */
  public static void save(InputStream content, Path archive) throws IOException {
    save(content, archive, MAX_BYTES);
  }

  static void save(InputStream content, Path archive, long maxBytes) throws IOException {
    try (OutputStream output = Files.newOutputStream(archive)) {
      writeContent(content, output, maxBytes);
    }
  }

  /**
   * Finds the class folders the development properties of an extracted archive name.
   *
   * @param directory the extracted archive
   *
   * @return the class folders in classpath order as the archive names them, an empty list when the
   *         properties name none, empty when the directory holds no development properties
   * @throws IOException if the development properties cannot be read
   */
  public static Optional<List<String>> findClassFolders(Path directory) throws IOException {
    Path file = directory.resolve(DEVELOPMENT_PROPERTIES);
    if (!Files.isRegularFile(file)) {
      return Optional.empty();
    }

    Properties properties = new Properties();
    try (Reader reader = Files.newBufferedReader(file, StandardCharsets.UTF_8)) {
      properties.load(reader);
    }

    String value = properties.getProperty(CLASS_FOLDER_KEY, "").strip();
    if (value.isEmpty()) {
      return Optional.of(List.of());
    }

    return Optional
        .of(Arrays.stream(value.split(File.pathSeparator, -1)).map(String::strip).toList());
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
    try (ZipInputStream zip = new ZipInputStream(Files.newInputStream(archive))) {
      if (zip.getNextEntry() == null) {
        throw new ZipException("The uploaded body is not a BBj deployment archive. Deploy the "
            + "ZIP archive the build writes");
      }
    }

    read(archive, directory, maxBytes, DeploymentArchive::applyNameValidation);
  }

  /**
   * Extracts an archive into a new directory, refusing an entry the validation refuses.
   *
   * @param archive the archive
   * @param directory the new directory
   * @param maxBytes the largest number of bytes the archive may unpack to
   * @param validation the validation of each entry name
   *
   * @throws IOException if an entry is refused, the archive is too large or cannot be extracted
   */
  static void read(Path archive, Path directory, long maxBytes, EntryValidation validation)
      throws IOException {
    Path destination = directory.toAbsolutePath().normalize();
    Files.createDirectory(destination);
    try {
      writeEntries(archive, destination, validation, maxBytes);
    } catch (IOException | RuntimeException failure) {
      try {
        FileTree.removeDirectory(destination);
      } catch (IOException cleanup) {
        failure.addSuppressed(cleanup);
      }

      throw failure;
    }
  }

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
   * Writes the development properties that name the class folders of a development run.
   *
   * <p>
   * The class folders are listed in classpath order, separated by the path separator of the BBj
   * machine.
   * </p>
   *
   * @param file the properties file
   * @param classFolders the absolute class folders in classpath order
   *
   * @throws IOException if the file cannot be written
   */
  static void writeClassFolders(Path file, List<Path> classFolders) throws IOException {
    Properties properties = new Properties();
    properties.setProperty(CLASS_FOLDER_KEY,
        classFolders.stream().map(Path::toString).collect(Collectors.joining(File.pathSeparator)));
    try (Writer writer = Files.newBufferedWriter(file, StandardCharsets.UTF_8)) {
      properties.store(writer, null);
    }
  }

  private static void writeEntries(Path archive, Path destination, EntryValidation validation,
      long maxBytes) throws IOException {
    Set<String> names = new HashSet<>();
    long remaining = maxBytes;
    try (ZipInputStream zip = new ZipInputStream(Files.newInputStream(archive))) {
      for (ZipEntry entry = zip.getNextEntry(); entry != null; entry = zip.getNextEntry()) {
        String name = entry.getName();
        validation.apply(name);

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

  static boolean isUnsafePath(String name) {
    String entry = name.endsWith("/") ? name.substring(0, name.length() - 1) : name;
    if (entry.isEmpty() || entry.contains("\\") || entry.contains(":") || entry.startsWith("/")) {
      return true;
    }

    for (String part : entry.split("/", -1)) {
      if (part.isEmpty() || ".".equals(part) || "..".equals(part)) {
        return true;
      }
    }

    return false;
  }

  private static void applyNameValidation(String name) throws IOException {
    boolean allowed = APPLICATION_ENTRY.equals(name) || DEVELOPMENT_PROPERTIES.equals(name)
        || name.startsWith(DEPENDENCIES_FOLDER + "/")
        || name.toLowerCase(Locale.ROOT).endsWith(".bbj");
    if (!allowed || isUnsafePath(name)) {
      throw new ZipException("Invalid BBj deployment archive entry: " + name);
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

  /**
   * Refuses an archive entry by its name.
   *
   * @author Hyyan Abo Fakher
   * @since 26.03
   */
  @FunctionalInterface
  interface EntryValidation {

    void apply(String name) throws IOException;
  }
}
