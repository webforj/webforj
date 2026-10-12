package com.webforj.plugin.foundation.bbj;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Stream;

/**
 * Finds a local BBj installation from the traces the installer and a running BBjServices leave on
 * the machine.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
final class Installation {

  static final String SERVICE_HOME_ARGUMENT = "-Dbasis.BBjHome=";
  static final Path INSTALL_RECORD = Path.of("bbj", "Install.properties");
  static final String INSTALL_RECORD_KEY = "InstallDirectory";
  static final List<String> EXECUTABLES = List.of("bbj", "bbj.exe");
  static final String CONFIGURATION = "cfg/config.bbx";

  private Installation() {}

  /**
   * Tells whether a directory holds a BBj installation.
   *
   * @param directory the directory
   *
   * @return whether the directory holds the BBj configuration file and an executable
   */
  static boolean isHome(Path directory) {
    return hasConfiguration(directory) && findExecutable(directory).isPresent();
  }

  /**
   * Tells whether a directory holds the BBj configuration file.
   *
   * @param directory the directory
   *
   * @return whether the directory holds the BBj configuration file
   */
  static boolean hasConfiguration(Path directory) {
    return Files.isDirectory(directory) && Files.isRegularFile(directory.resolve(CONFIGURATION));
  }

  /**
   * Finds the bbj executable of an installation.
   *
   * @param directory the installation directory
   *
   * @return the executable, empty when the bin folder holds no executable bbj file
   */
  static Optional<Path> findExecutable(Path directory) {
    Path bin = directory.resolve("bin");

    return EXECUTABLES.stream().map(bin::resolve).filter(Installation::isExecutableFile)
        .findFirst();
  }

  /**
   * Finds the installation a running BBjServices was started from.
   *
   * @param processes the processes of the machine
   *
   * @return the installation, empty when no running process names one
   */
  static Optional<Path> findInProcesses(Stream<ProcessHandle> processes) {
    return processes.map(process -> process.info().arguments()).flatMap(Optional::stream)
        .flatMap(Arrays::stream).filter(argument -> argument.startsWith(SERVICE_HOME_ARGUMENT))
        .map(argument -> Path.of(argument.substring(SERVICE_HOME_ARGUMENT.length())))
        .filter(Installation::isHome).findFirst();
  }

  /**
   * Finds the installation the BASIS installer recorded for the user who ran it.
   *
   * @param userHome the home directory of the user
   *
   * @return the installation, empty when the install file is missing or names no installation
   */
  static Optional<Path> findInInstallRecord(Path userHome) {
    Path installFile = userHome.resolve(INSTALL_RECORD);
    if (!Files.isRegularFile(installFile)) {
      return Optional.empty();
    }

    Properties properties = new Properties();
    try (InputStream input = Files.newInputStream(installFile)) {
      properties.load(input);
    } catch (IOException | IllegalArgumentException failure) {
      return Optional.empty();
    }

    String directory = properties.getProperty(INSTALL_RECORD_KEY, "");

    return directory.isBlank() ? Optional.empty()
        : Optional.of(Path.of(directory)).filter(Installation::isHome);
  }

  /**
   * Finds the installation of a bbj executable on the search path of the shell.
   *
   * @param path the search path, directories separated as the operating system does
   *
   * @return the installation, empty when no directory on the path holds a bbj executable
   */
  static Optional<Path> findOnPath(String path) {
    if (path == null) {
      return Optional.empty();
    }

    return Arrays.stream(path.split(File.pathSeparator)).filter(directory -> !directory.isBlank())
        .map(Path::of).flatMap(directory -> EXECUTABLES.stream().map(directory::resolve))
        .filter(Installation::isExecutableFile).map(Installation::findExecutableHome)
        .flatMap(Optional::stream).findFirst();
  }

  /**
   * Finds the installation in the default folder of the installer.
   *
   * @param osName the operating system name
   * @param root the root the default folder is resolved against
   *
   * @return the installation, empty when the default folder holds none
   */
  static Optional<Path> findDefault(String osName, Path root) {
    String name = osName == null ? "" : osName.toLowerCase(Locale.ROOT);
    Path home;
    if (name.contains("mac")) {
      home = root.resolve("Applications").resolve("bbx");
    } else if (name.contains("windows")) {
      home = root.resolve("bbx");
    } else {
      home = root.resolve("usr").resolve("local").resolve("bbx");
    }

    return Optional.of(home).filter(Installation::isHome);
  }

  private static boolean isExecutableFile(Path file) {
    return Files.isRegularFile(file) && Files.isExecutable(file);
  }

  private static Optional<Path> findExecutableHome(Path executable) {
    try {
      Path bin = executable.toRealPath().getParent();

      return Optional.ofNullable(bin == null ? null : bin.getParent()).filter(Installation::isHome);
    } catch (IOException failure) {
      return Optional.empty();
    }
  }
}
