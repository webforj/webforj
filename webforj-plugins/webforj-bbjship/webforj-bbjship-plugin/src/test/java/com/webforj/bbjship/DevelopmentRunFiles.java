package com.webforj.bbjship;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * Writes the development properties of an extracted archive that name the class folders of a
 * development run.
 */
public final class DevelopmentRunFiles {

  private DevelopmentRunFiles() {}

  /**
   * Writes the development properties naming the class folders in classpath order.
   *
   * @param directory the deployment directory
   * @param classFolders the class folders
   *
   * @throws IOException if the file cannot be written
   */
  public static void write(Path directory, Path... classFolders) throws IOException {
    String value = Arrays.stream(classFolders).map(Path::toString)
        .collect(Collectors.joining(File.pathSeparator));
    Files.writeString(directory.resolve("development.properties"),
        "classFolder=" + value.replace("\\", "\\\\") + System.lineSeparator());
  }
}
