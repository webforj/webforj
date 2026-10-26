package com.webforj.testbed.snapshot;

import com.github.difflib.DiffUtils;
import com.github.difflib.UnifiedDiffUtils;
import com.github.difflib.patch.Patch;
import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.Component;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Optional;
import java.util.OptionalInt;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Stores and verifies the snapshots of component trees.
 *
 * <p>
 * A snapshot file lives under the snapshot directory, in the folders of the package of the test
 * class, and is named after the test class followed by the test method or the explicit name the
 * assertion passed. The first local run writes it, later runs compare against it. On a continuous
 * integration server a missing snapshot fails instead of being written, and snapshots are never
 * updated there.
 * </p>
 *
 * @author Hyyan Abo Fakher
 * @since 26.03
 */
public final class SnapshotStore {

  /**
   * The system property that rewrites every verified snapshot when set to true.
   */
  public static final String UPDATE_PROPERTY = "webforj.testbed.updateSnapshots";

  /**
   * The system property that moves the snapshot directory.
   */
  public static final String DIRECTORY_PROPERTY = "webforj.testbed.snapshotDirectory";

  /**
   * The system property that forces or disables the continuous integration mode. Without it the
   * mode follows the {@code CI} environment variable.
   */
  public static final String CI_PROPERTY = "webforj.testbed.ci";

  static final String DEFAULT_DIRECTORY = "src/test/resources/webforj-snapshots";
  private static final int VERSION = 1;
  private static final String HEADER_PREFIX = "# webforJ testbed snapshot v";
  private static final String HEADER = HEADER_PREFIX + VERSION;
  private static final String EXTENSION = ".yml";
  private static final String MESSAGE_START = "Snapshot <";
  private static final ThreadLocal<Session> current = new ThreadLocal<>();

  private SnapshotStore() {}

  /**
   * Verifies the tree below the given component against its stored snapshot.
   *
   * @param component the root of the tree
   * @param name the snapshot name, null for the name of the running test method
   * @param options the rendering options
   *
   * @return the failure message, empty when the snapshot matches or was written
   */
  public static Optional<String> verify(Component component, String name, SnapshotOptions options) {
    Session session = sessionOrCaller();
    String snapshotName = name != null ? sanitize(name) : session.testMethod;
    session.verified.add(snapshotName);

    Path file =
        folderOf(session.testClass).resolve(prefixOf(session.testClass) + snapshotName + EXTENSION);
    String rendered = HEADER + "\n" + Snapshot.render(component, options);
    boolean update = Boolean.getBoolean(UPDATE_PROPERTY);
    if (update && isCi()) {
      return Optional.of("Snapshots cannot be updated on a continuous integration server. Update "
          + "them locally and commit the result.");
    }

    if (!Files.exists(file)) {
      if (isCi()) {
        return Optional
            .of(MESSAGE_START + snapshotName + "> is missing at " + file + ". Snapshots are "
                + "written locally and committed, never on a continuous integration server.");
      }

      write(file, rendered);

      return Optional.empty();
    }

    if (update) {
      write(file, rendered);

      return Optional.empty();
    }

    String stored = normalize(read(file));
    OptionalInt version = versionOf(stored);
    if (version.isEmpty() || version.getAsInt() != VERSION) {
      return Optional.of(versionMessage(snapshotName, file, version));
    }

    String storedBody = bodyOf(stored);
    String renderedBody = bodyOf(normalize(rendered));
    if (storedBody.equals(renderedBody)) {
      return Optional.empty();
    }

    return Optional.of(mismatchMessage(snapshotName, file, storedBody, renderedBody));
  }

  /**
   * Starts recording the snapshots the given test method verifies.
   *
   * @param testClass the test class
   * @param testMethod the test method name
   */
  public static void beginTest(Class<?> testClass, String testMethod) {
    current.set(new Session(testClass.getName(), testMethod));
  }

  /**
   * Stops recording and returns the names of the snapshots the test verified.
   *
   * @return the verified snapshot names
   */
  public static Set<String> endTest() {
    Session session = current.get();
    current.remove();

    return session == null ? Set.of() : Set.copyOf(session.verified);
  }

  /**
   * Finds the snapshot files of the given test class that none of its tests verified. In update
   * mode the files are deleted, otherwise they are reported.
   *
   * @param testClass the test class
   * @param verified the names of the snapshots the tests of the class verified
   *
   * @return the failure message naming the obsolete files, empty when there are none or they were
   *         deleted
   */
  public static Optional<String> findObsolete(Class<?> testClass, Set<String> verified) {
    Path folder = folderOf(testClass.getName());
    if (!Files.isDirectory(folder)) {
      return Optional.empty();
    }

    String prefix = prefixOf(testClass.getName());
    List<Path> obsolete = new ArrayList<>();
    try (Stream<Path> files = Files.list(folder)) {
      files.filter(file -> isSnapshotOf(prefix, file)).sorted().forEach(file -> {
        String name = file.getFileName().toString();
        if (!verified
            .contains(name.substring(prefix.length(), name.length() - EXTENSION.length()))) {
          obsolete.add(file);
        }
      });
    } catch (IOException e) {
      throw new UncheckedIOException("Failed to list the snapshot folder " + folder, e);
    }

    if (obsolete.isEmpty()) {
      return Optional.empty();
    }

    if (Boolean.getBoolean(UPDATE_PROPERTY) && !isCi()) {
      obsolete.forEach(SnapshotStore::delete);

      return Optional.empty();
    }

    return Optional.of(obsoleteMessage(testClass, obsolete));
  }

  static boolean isCi() {
    String property = System.getProperty(CI_PROPERTY);
    if (property != null) {
      return Boolean.parseBoolean(property);
    }

    String environment = System.getenv("CI");

    return environment != null && !environment.isBlank() && !environment.equalsIgnoreCase("false")
        && !environment.equals("0");
  }

  private static OptionalInt versionOf(String content) {
    String firstLine = content.lines().findFirst().orElse("");
    if (!firstLine.startsWith(HEADER_PREFIX)) {
      return OptionalInt.empty();
    }

    try {
      return OptionalInt.of(Integer.parseInt(firstLine.substring(HEADER_PREFIX.length()).trim()));
    } catch (NumberFormatException e) {
      return OptionalInt.empty();
    }
  }

  private static String bodyOf(String content) {
    int lineBreak = content.indexOf('\n');

    return lineBreak == -1 ? "" : content.substring(lineBreak + 1);
  }

  private static String versionMessage(String name, Path file, OptionalInt version) {
    String written = version.isEmpty() ? "an unknown format" : "format v" + version.getAsInt();

    return MESSAGE_START + name + "> at " + file + " was written with " + written
        + " and this testbed writes format v" + VERSION + ". Run with -D" + UPDATE_PROPERTY
        + "=true to rewrite it, then review the diff.";
  }

  private static String mismatchMessage(String name, Path file, String stored, String rendered) {
    return MESSAGE_START + name + "> does not match " + file + "." + System.lineSeparator()
        + "First difference at line " + firstDifference(stored, rendered) + "."
        + System.lineSeparator() + unifiedDiff(stored, rendered) + "Actual:"
        + System.lineSeparator() + rendered + "Run with -D" + UPDATE_PROPERTY
        + "=true to update the snapshot, or verify more than one snapshot in the same test with"
        + " explicit names.";
  }

  // The diff reads like a pull request: the stored lines that went away, the actual lines that
  // replaced them, two lines of context around each change.
  private static String unifiedDiff(String stored, String rendered) {
    List<String> storedLines = stored.lines().toList();
    List<String> renderedLines = rendered.lines().toList();
    Patch<String> patch = DiffUtils.diff(storedLines, renderedLines);
    List<String> diff =
        UnifiedDiffUtils.generateUnifiedDiff("stored", "actual", storedLines, patch, 2);

    return String.join(System.lineSeparator(), diff) + System.lineSeparator();
  }

  private static String obsoleteMessage(Class<?> testClass, List<Path> obsolete) {
    StringBuilder message = new StringBuilder("The tests of ").append(testClass.getName())
        .append(" verify none of these snapshots:");
    for (Path file : obsolete) {
      message.append(System.lineSeparator()).append("  ").append(file);
    }

    return message.append(System.lineSeparator()).append("Delete them, or run with -D")
        .append(UPDATE_PROPERTY).append("=true to delete them.").toString();
  }

  private static int firstDifference(String stored, String rendered) {
    String[] storedLines = stored.split("\n", -1);
    String[] renderedLines = rendered.split("\n", -1);
    int lines = Math.min(storedLines.length, renderedLines.length);
    for (int i = 0; i < lines; i++) {
      if (!storedLines[i].equals(renderedLines[i])) {
        return i + 1;
      }
    }

    return lines + 1;
  }

  private static Path directory() {
    return Path.of(System.getProperty(DIRECTORY_PROPERTY, DEFAULT_DIRECTORY));
  }

  // The folders follow the package of the test class the way the test sources do.
  private static Path folderOf(String testClass) {
    Path folder = directory();
    int split = testClass.lastIndexOf('.');
    if (split == -1) {
      return folder;
    }

    for (String segment : testClass.substring(0, split).split("\\.")) {
      folder = folder.resolve(segment);
    }

    return folder;
  }

  // The class name leads the file name, so the snapshots of the classes of a package sit side by
  // side and the ones of a class are told apart from the ones of its neighbors.
  private static String prefixOf(String testClass) {
    return testClass.substring(testClass.lastIndexOf('.') + 1) + ".";
  }

  private static boolean isSnapshotOf(String prefix, Path file) {
    String name = file.getFileName().toString();

    return Files.isRegularFile(file) && name.startsWith(prefix) && name.endsWith(EXTENSION);
  }

  private static String normalize(String text) {
    String unified = text.replace("\r\n", "\n");

    return unified.endsWith("\n") ? unified : unified + "\n";
  }

  private static String sanitize(String name) {
    String sanitized = name.trim().replaceAll("[^A-Za-z0-9._ -]", "-");
    if (sanitized.isEmpty()) {
      throw new IllegalArgumentException("The snapshot name cannot be blank");
    }

    return sanitized;
  }

  private static void write(Path file, String content) {
    try {
      Files.createDirectories(file.getParent());
      Files.writeString(file, content, StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new UncheckedIOException("Failed to write the snapshot file " + file, e);
    }
  }

  private static String read(Path file) {
    try {
      return Files.readString(file, StandardCharsets.UTF_8);
    } catch (IOException e) {
      throw new UncheckedIOException("Failed to read the snapshot file " + file, e);
    }
  }

  private static void delete(Path file) {
    try {
      Files.delete(file);
    } catch (IOException e) {
      throw new UncheckedIOException("Failed to delete the snapshot file " + file, e);
    }
  }

  // A test running through the testbed extension has a session. A test that sets the testbed up by
  // hand has none, so the calling test method names the snapshot and nothing tracks obsolete files.
  private static Session sessionOrCaller() {
    Session session = current.get();
    if (session != null) {
      return session;
    }

    return StackWalker.getInstance()
        .walk(frames -> frames
            .filter(frame -> !frame.getClassName().equals(SnapshotStore.class.getName())
                && !frame.getClassName().startsWith("com.webforj.testbed.assertion."))
            .findFirst().map(frame -> new Session(frame.getClassName(), frame.getMethodName())))
        .orElseThrow(() -> new IllegalStateException("Failed to resolve the calling test method"));
  }

  private static final class Session {
    private final String testClass;
    private final String testMethod;
    private final Set<String> verified = new LinkedHashSet<>();

    private Session(String testClass, String testMethod) {
      this.testClass = testClass;
      this.testMethod = testMethod;
    }
  }
}
