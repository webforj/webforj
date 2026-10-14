package com.webforj.bbjship.foundation;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Properties;

/**
 * Reads and writes the Java settings of BBjServices in its properties file.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ServicesProperties {

  /**
   * The key naming the java executable BBjServices runs on.
   */
  public static final String JAVA_EXECUTABLE_KEY = "basis.java.jvm.BBjServices";

  /**
   * The key naming the default java executable of BBj.
   */
  public static final String DEFAULT_JAVA_EXECUTABLE_KEY = "basis.java.jvm.Default";

  /**
   * The key holding the Java arguments BBjServices runs with.
   */
  public static final String JAVA_ARGUMENTS_KEY = "basis.java.args.BBjServices";

  static final String BACKUP_SUFFIX = ".before-webforj";
  private static final String DEFAULT_FILE = "cfg/BBj.properties";
  private static final String OPTIONS_FILE_PREFIX = "-XX:VMOptionsFile=";
  private static final String WINDOWS_LIBRARY = "jvm.dll";
  private static final String WINDOWS_EXECUTABLE = "java.exe";

  private final Path file;
  private final Path backupDirectory;
  private final Properties properties;

  private ServicesProperties(Path file, Path backupDirectory, Properties properties) {
    this.file = file;
    this.backupDirectory = backupDirectory;
    this.properties = properties;
  }

  /**
   * Loads the properties file of BBjServices.
   *
   * @param file the properties file
   * @param backupDirectory the folder that holds the copy taken before the first change
   *
   * @return the loaded properties
   * @throws IOException if the file is missing or cannot be read
   */
  public static ServicesProperties load(Path file, Path backupDirectory) throws IOException {
    if (!Files.isRegularFile(file)) {
      throw new IOException(
          "BBjServices properties file not found at " + file + ". Configure bbj.properties");
    }

    return new ServicesProperties(file, backupDirectory, readProperties(file));
  }

  /**
   * Finds the java executable BBjServices runs on, as its properties file names it.
   *
   * @param home the BBj home, null to detect it
   * @param file the properties file of BBjServices, null for the file in the cfg folder of the home
   *
   * @return the java executable of BBjServices, or the default java executable of BBj, empty when
   *         the properties file is missing or names neither
   * @throws IOException if the BBj home cannot be resolved when no file is named, or the properties
   *         file cannot be read
   */
  public static Optional<Path> findJavaExecutable(Path home, Path file) throws IOException {
    Path properties = file != null ? file : resolveDefaultFile(Installation.resolveHome(home));
    if (!Files.isRegularFile(properties)) {
      return Optional.empty();
    }

    ServicesProperties services =
        new ServicesProperties(properties, null, readProperties(properties));

    return services.getJavaExecutable().or(services::getDefaultJavaExecutable);
  }

  /**
   * Resolves the properties file of BBjServices in the cfg folder of a BBj home.
   *
   * @param home the BBj home
   *
   * @return the default properties file
   */
  public static Path resolveDefaultFile(Path home) {
    return home.resolve(DEFAULT_FILE);
  }

  /**
   * Resolves the copy of a properties file taken before its first change.
   *
   * @param file the properties file
   * @param backupDirectory the folder that holds the copy
   *
   * @return the copy of the properties file
   */
  public static Path resolveBackup(Path file, Path backupDirectory) {
    return backupDirectory.resolve(file.getFileName() + BACKUP_SUFFIX);
  }

  /**
   * Gets the java executable BBjServices runs on. A virtual machine library named on Windows
   * resolves to the java executable of its Java home.
   *
   * @return the java executable, empty when the file names none
   */
  public Optional<Path> getJavaExecutable() {
    return readPath(JAVA_EXECUTABLE_KEY).map(ServicesProperties::toExecutable);
  }

  /**
   * Gets the default java executable of BBj. A virtual machine library named on Windows resolves to
   * the java executable of its Java home.
   *
   * @return the java executable, empty when the file names none
   */
  public Optional<Path> getDefaultJavaExecutable() {
    return readPath(DEFAULT_JAVA_EXECUTABLE_KEY).map(ServicesProperties::toExecutable);
  }

  /**
   * Gets the Java arguments BBjServices runs with.
   *
   * @return the arguments split on whitespace, empty when the file names none
   */
  public List<String> getJavaArguments() {
    String value = properties.getProperty(JAVA_ARGUMENTS_KEY, "");

    return value.isBlank() ? List.of() : List.of(value.strip().split("\\s+"));
  }

  /**
   * Checks whether BBjServices is set to run with an argument.
   *
   * @param argument the argument
   *
   * @return true when the file holds the argument
   */
  public boolean hasJavaSettings(String argument) {
    return getJavaArguments().contains(argument);
  }

  /**
   * Sets BBjServices to run with an options file argument.
   *
   * <p>
   * The argument replaces the options file argument of the file or is added after the other
   * arguments. Every other argument and key is kept. A copy of the file is taken into the backup
   * folder before its first change. Nothing is written when the settings are already present.
   * </p>
   *
   * @param argument the argument naming the options file
   *
   * @throws IOException if the file cannot be written
   */
  public void applyJavaSettings(String argument) throws IOException {
    if (hasJavaSettings(argument)) {
      return;
    }

    List<String> arguments = new ArrayList<>();
    boolean replaced = false;
    for (String current : getJavaArguments()) {
      if (!current.startsWith(OPTIONS_FILE_PREFIX)) {
        arguments.add(current);
      } else if (!replaced) {
        arguments.add(argument);
        replaced = true;
      }
    }

    if (!replaced) {
      arguments.add(argument);
    }

    Path backup = resolveBackup(file, backupDirectory);
    if (!Files.exists(backup)) {
      Files.createDirectories(backupDirectory);
      Files.copy(file, backup);
    }

    properties.setProperty(JAVA_ARGUMENTS_KEY, String.join(" ", arguments));
    writeProperties();
  }

  /**
   * Checks whether BBjServices is set to run with the hotswap options file argument.
   *
   * @return true when the file holds an argument naming a hotswap options file
   */
  public boolean hasHotswapArgument() {
    return getJavaArguments().stream().anyMatch(ContainerOptionsFile::isArgument);
  }

  /**
   * Takes the Java settings of BBjServices back to the state before the first change.
   *
   * <p>
   * The copy taken before the first change replaces the file and is removed. Without a copy the
   * hotswap options file argument is removed from the Java arguments and every other argument and
   * key is kept. Nothing is touched when there is neither a copy nor that argument.
   * </p>
   *
   * @param file the properties file
   * @param backupDirectory the folder that holds the copy taken before the first change
   *
   * @return true when the file was changed, false when nothing was to reset
   * @throws IOException if the copy cannot be moved over the file, or the file cannot be read or
   *         written
   */
  public static boolean resetJavaSettings(Path file, Path backupDirectory) throws IOException {
    Path backup = resolveBackup(file, backupDirectory);
    if (Files.exists(backup)) {
      Files.move(backup, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
      return true;
    }

    if (!Files.isRegularFile(file)) {
      return false;
    }

    ServicesProperties services =
        new ServicesProperties(file, backupDirectory, readProperties(file));
    if (!services.hasHotswapArgument()) {
      return false;
    }

    List<String> arguments = services.getJavaArguments().stream()
        .filter(argument -> !ContainerOptionsFile.isArgument(argument)).toList();
    services.properties.setProperty(JAVA_ARGUMENTS_KEY, String.join(" ", arguments));
    services.writeProperties();

    return true;
  }

  private static Properties readProperties(Path file) throws IOException {
    Properties properties = new Properties();
    try (InputStream input = Files.newInputStream(file)) {
      properties.load(input);
    }

    return properties;
  }

  private void writeProperties() throws IOException {
    try (OutputStream output = Files.newOutputStream(file)) {
      properties.store(output, null);
    }
  }

  private Optional<Path> readPath(String key) {
    String value = properties.getProperty(key, "");
    return value.isBlank() ? Optional.empty() : Optional.of(Path.of(value.strip()));
  }

  private static Path toExecutable(Path java) {
    // BBj on Windows names the virtual machine library, <java home>\bin\server\jvm.dll, in place
    // of the executable. The executable of that Java home is <java home>\bin\java.exe
    Path name = java.getFileName();
    Path bin = java.getParent() == null ? null : java.getParent().getParent();
    if (name == null || bin == null || !name.toString().equalsIgnoreCase(WINDOWS_LIBRARY)) {
      return java;
    }

    return bin.resolve(WINDOWS_EXECUTABLE);
  }
}
