package com.webforj.devtools.craftforj.history;

import java.io.IOException;
import java.io.OutputStream;
import java.lang.System.Logger;
import java.lang.System.Logger.Level;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

/**
 * Writes the files of the running project.
 *
 * <p>
 * Every change craftforJ makes to a project file goes through this class, which adds the file to
 * the history step of the current request before the file changes.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class ProjectFileWriter {

  static final String TEMP_PREFIX = ".";
  static final String TEMP_SUFFIX = ".craftforj";

  private static final Logger LOGGER = System.getLogger(ProjectFileWriter.class.getName());
  private static final ThreadLocal<HistoryStep> STEP = new ThreadLocal<>();

  private ProjectFileWriter() {}

  /**
   * Writes the content over the file, creating the file when it does not exist.
   *
   * @param file the file
   * @param content the content, written as UTF-8
   * @throws IOException when the file cannot be written
   */
  public static void write(Path file, String content) throws IOException {
    addStepFile(file);
    Files.writeString(file, content, StandardCharsets.UTF_8);
  }

  /**
   * Writes the content over the file in one move, creating the file and its folders when they do
   * not exist.
   *
   * <p>
   * A reader never sees half of the content. A file that exists keeps its permissions.
   * </p>
   *
   * @param file the file
   * @param content the content, written as UTF-8
   * @throws IOException when the file cannot be written
   */
  public static void writeAtomic(Path file, String content) throws IOException {
    Path parent = file.toAbsolutePath().getParent();
    if (parent != null) {
      Files.createDirectories(parent);
    }

    addStepFile(file);
    writeFile(file, content.getBytes(StandardCharsets.UTF_8));
  }

  /**
   * Removes the file when it exists.
   *
   * @param file the file
   * @return {@code true} when the file was removed
   * @throws IOException when the file cannot be removed
   */
  public static boolean remove(Path file) throws IOException {
    addStepFile(file);
    return Files.deleteIfExists(file);
  }

  static void setStep(HistoryStep step) {
    STEP.set(step);
  }

  static void clearStep() {
    STEP.remove();
  }

  /**
   * Resets the file to the given content without adding it to the open step.
   *
   * @param file the file
   * @param content the content, or {@code null} to remove the file
   * @param mode the permissions a file that has to be created takes, or {@code null} for the
   *        default
   * @throws IOException when the file cannot be written
   */
  static void reset(Path file, byte[] content, Set<PosixFilePermission> mode) throws IOException {
    if (content == null) {
      Files.deleteIfExists(file);
      return;
    }

    Path target = Files.isSymbolicLink(file) ? file.toRealPath() : file;
    if (Files.exists(target)) {
      writeFile(target, content);
    } else {
      createFile(target, content);
      applyPermissions(target, mode);
    }
  }

  static Optional<Set<PosixFilePermission>> readPermissions(Path file) {
    try {
      if (Files.exists(file)
          && Files.getFileStore(file).supportsFileAttributeView(PosixFileAttributeView.class)) {
        return Optional.of(Files.getPosixFilePermissions(file));
      }
    } catch (IOException | UnsupportedOperationException e) {
      LOGGER.log(Level.DEBUG, "Could not read the permissions of " + file, e);
    }

    return Optional.empty();
  }

  private static void addStepFile(Path file) {
    HistoryStep step = STEP.get();
    if (step != null) {
      step.addFile(file);
    }
  }

  private static void writeFile(Path file, byte[] content) throws IOException {
    Optional<Set<PosixFilePermission>> mode = readPermissions(file);
    Path temp = file.toAbsolutePath()
        .resolveSibling(TEMP_PREFIX + file.getFileName() + "." + UUID.randomUUID() + TEMP_SUFFIX);
    try {
      Files.write(temp, content, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
      mode.ifPresent(permissions -> applyPermissions(temp, permissions));
      try {
        Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
      } catch (AtomicMoveNotSupportedException e) {
        Files.move(temp, file, StandardCopyOption.REPLACE_EXISTING);
      }
    } finally {
      Files.deleteIfExists(temp);
    }
  }

  private static void createFile(Path file, byte[] content) throws IOException {
    Files.createDirectories(file.toAbsolutePath().getParent());
    OutputStream out =
        Files.newOutputStream(file, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
    try (out) {
      out.write(content);
    } catch (IOException | RuntimeException e) {
      Files.deleteIfExists(file);
      throw e;
    }
  }

  private static void applyPermissions(Path file, Set<PosixFilePermission> mode) {
    if (mode == null) {
      return;
    }

    try {
      Files.setPosixFilePermissions(file, mode);
    } catch (IOException | UnsupportedOperationException e) {
      LOGGER.log(Level.DEBUG, "Could not set the permissions of " + file, e);
    }
  }
}
