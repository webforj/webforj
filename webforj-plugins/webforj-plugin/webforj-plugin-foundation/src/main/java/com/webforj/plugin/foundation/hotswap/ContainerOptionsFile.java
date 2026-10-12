package com.webforj.plugin.foundation.hotswap;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Writes and reads the options file a shared container virtual machine reads its hotswap arguments
 * from.
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ContainerOptionsFile {

  private static final String FILE_NAME = "hotswap.options";

  private ContainerOptionsFile() {}

  /**
   * Writes the options file into a directory, one argument per line.
   *
   * <p>
   * A line holding whitespace is wrapped in double quotes. The file is written only when its
   * content differs from the file on disk.
   * </p>
   *
   * @param directory the directory the file is written into
   * @param lines the arguments
   *
   * @return true when an existing file with other content was replaced
   * @throws IOException if a line holds a double quote or the file cannot be written
   */
  public static boolean write(Path directory, List<String> lines) throws IOException {
    List<String> formatted = new ArrayList<>();
    for (String line : lines) {
      formatted.add(formatLine(line));
    }

    return writeContent(getFile(directory), String.join("\n", formatted) + "\n");
  }

  /**
   * Reads the arguments of the options file in a directory.
   *
   * @param directory the directory the file was written into
   *
   * @return the arguments, one per line of the file without blank lines and surrounding double
   *         quotes
   * @throws IOException if the file cannot be read
   */
  public static List<String> readLines(Path directory) throws IOException {
    List<String> lines = new ArrayList<>();
    for (String line : Files.readAllLines(getFile(directory))) {
      if (!line.isBlank()) {
        lines.add(parseLine(line.strip()));
      }
    }

    return List.copyOf(lines);
  }

  /**
   * Gets the options file of a directory.
   *
   * @param directory the directory the file is written into
   *
   * @return the options file
   */
  static Path getFile(Path directory) {
    return directory.resolve(FILE_NAME);
  }

  /**
   * Writes a settings file of the container only when its content differs from the file on disk.
   *
   * @param file the file
   * @param content the content
   *
   * @return true when an existing file with other content was replaced
   * @throws IOException if the file cannot be read or written
   */
  public static boolean writeContent(Path file, String content) throws IOException {
    boolean existed = Files.exists(file);
    if (existed && content.equals(Files.readString(file))) {
      return false;
    }

    Files.createDirectories(file.toAbsolutePath().getParent());
    Files.writeString(file, content);

    return existed;
  }

  /**
   * Gets the virtual machine argument that names the options file of a directory.
   *
   * @param directory the directory the file is written into
   *
   * @return the argument
   */
  public static String getArgument(Path directory) {
    return "-XX:VMOptionsFile=" + getFile(directory).toAbsolutePath();
  }

  private static String formatLine(String line) throws IOException {
    if (line.indexOf('"') >= 0) {
      throw new IOException("the options file cannot contain a double quote: " + line);
    }

    for (int index = 0; index < line.length(); index++) {
      if (Character.isWhitespace(line.charAt(index))) {
        return "\"" + line + "\"";
      }
    }

    return line;
  }

  private static String parseLine(String line) {
    if (line.length() > 1 && line.startsWith("\"") && line.endsWith("\"")) {
      return line.substring(1, line.length() - 1);
    }

    return line;
  }
}
