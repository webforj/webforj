package com.webforj.testbed.snapshot;

import static com.webforj.testbed.assertion.WebforjAssertions.verifyThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.button.Button;
import com.webforj.component.element.Element;
import com.webforj.component.field.TextField;
import com.webforj.component.html.elements.Div;
import com.webforj.component.layout.flexlayout.FlexLayout;
import com.webforj.testbed.TestbedTestSupport;
import com.webforj.testbed.WebforjTestbed;
import com.webforj.testbed.assertion.ComponentAssert;
import com.webforj.testbed.fixture.CustomPicker;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class SnapshotTest extends TestbedTestSupport {

  @TempDir
  Path snapshotDirectory;

  private String previousDirectory;
  private String previousCi;
  private String previousUpdate;

  @BeforeEach
  void pointSnapshotsToTheTempDirectory() {
    previousDirectory = System.getProperty(SnapshotStore.DIRECTORY_PROPERTY);
    previousCi = System.getProperty(SnapshotStore.CI_PROPERTY);
    previousUpdate = System.getProperty(SnapshotStore.UPDATE_PROPERTY);
    System.setProperty(SnapshotStore.DIRECTORY_PROPERTY, snapshotDirectory.toString());
    System.setProperty(SnapshotStore.CI_PROPERTY, "false");
    System.clearProperty(SnapshotStore.UPDATE_PROPERTY);
  }

  @AfterEach
  void restoreTheSnapshotSettings() {
    restore(SnapshotStore.UPDATE_PROPERTY, previousUpdate);
    restore(SnapshotStore.DIRECTORY_PROPERTY, previousDirectory);
    restore(SnapshotStore.CI_PROPERTY, previousCi);
  }

  @Test
  void shouldWriteTheSnapshotWithTheFormatVersionOnTheFirstRun() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);

    verifyThat(button).matchesSnapshot("first-run");

    assertEquals(List.of("# webforJ testbed snapshot v1", "- Button \"Save\""),
        readLines(snapshotFile("first-run")));
  }

  @Test
  void shouldPassWhenTheTreeMatchesTheStoredSnapshot() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);

    verifyThat(button).matchesSnapshot("stable");
    verifyThat(button).matchesSnapshot("stable");
  }

  @Test
  void shouldFailWhenTheTreeChanged() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);
    verifyThat(button).matchesSnapshot("changed");

    button.setText("Delete");

    ComponentAssert assertion = verifyThat(button);
    AssertionError failure =
        assertThrows(AssertionError.class, () -> assertion.matchesSnapshot("changed"));
    assertTrue(failure.getMessage().contains("changed"));
    assertTrue(failure.getMessage().contains("First difference at line 1"));
    assertTrue(failure.getMessage().contains("--- stored"));
    assertTrue(failure.getMessage().contains("+++ actual"));
    assertTrue(failure.getMessage().contains("-- Button \"Save\""));
    assertTrue(failure.getMessage().contains("+- Button \"Delete\""));
    assertTrue(failure.getMessage().contains(SnapshotStore.UPDATE_PROPERTY));
  }

  @Test
  void shouldRewriteTheSnapshotWhenTheUpdatePropertyIsSet() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);
    verifyThat(button).matchesSnapshot("updated");

    button.setText("Delete");
    System.setProperty(SnapshotStore.UPDATE_PROPERTY, "true");
    verifyThat(button).matchesSnapshot("updated");

    System.clearProperty(SnapshotStore.UPDATE_PROPERTY);
    verifyThat(button).matchesSnapshot("updated");
    assertEquals("- Button \"Delete\"", readLines(snapshotFile("updated")).get(1));
  }

  @Test
  void shouldReportTheStoredSnapshotOfAnotherFormatVersion() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);
    write(snapshotFile("old-format"), "Button text=Save\n");
    write(snapshotFile("future-format"), "# webforJ testbed snapshot v3\n- Button \"Save\"\n");

    ComponentAssert assertion = verifyThat(button);
    AssertionError unknown =
        assertThrows(AssertionError.class, () -> assertion.matchesSnapshot("old-format"));
    AssertionError future =
        assertThrows(AssertionError.class, () -> assertion.matchesSnapshot("future-format"));

    assertTrue(unknown.getMessage().contains("an unknown format"));
    assertTrue(future.getMessage().contains("format v3"));
    assertTrue(future.getMessage().contains("writes format v1"));
    assertFalse(future.getMessage().contains("First difference"));
  }

  @Test
  void shouldFailOnTheMissingSnapshotOnCi() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);
    System.setProperty(SnapshotStore.CI_PROPERTY, "true");

    ComponentAssert assertion = verifyThat(button);
    AssertionError failure =
        assertThrows(AssertionError.class, () -> assertion.matchesSnapshot("ci"));

    assertTrue(failure.getMessage().contains("missing"));
    assertFalse(Files.exists(snapshotFile("ci")));
  }

  @Test
  void shouldRefuseToUpdateSnapshotsOnCi() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);
    verifyThat(button).matchesSnapshot("ci-update");
    System.setProperty(SnapshotStore.CI_PROPERTY, "true");
    System.setProperty(SnapshotStore.UPDATE_PROPERTY, "true");

    ComponentAssert assertion = verifyThat(button);
    AssertionError failure =
        assertThrows(AssertionError.class, () -> assertion.matchesSnapshot("ci-update"));

    assertTrue(failure.getMessage().contains("cannot be updated"));
  }

  @Test
  void shouldDeriveTheSnapshotNameFromTheTestMethod() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);

    verifyThat(button).matchesSnapshot();

    assertTrue(Files.exists(snapshotFile("shouldDeriveTheSnapshotNameFromTheTestMethod")));
  }

  @Test
  void shouldSanitizeTheSnapshotName() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Button button = new Button("Save");
    context.add(button);

    verifyThat(button).matchesSnapshot("save/form:v2");

    assertTrue(Files.exists(snapshotFile("save-form-v2")));
  }

  @Test
  void shouldSnapshotWhatTheApplicationComposed() {
    FlexLayout form = new FlexLayout();
    form.setName("login-form");
    TextField name = new TextField();
    name.setName("name");
    name.setLabel("Name");
    name.setText("Ada");
    name.setRequired(true);
    Button cancel = new Button("Cancel");
    cancel.setVisible(false);
    Div note = new Div();
    note.setText("Say \"hi\": now");
    Element raw = new Element("section");
    form.add(name, new Button("Save"), cancel, note, raw);
    WebforjTestbed.getCurrent().add(form);

    verifyThat(form).matchesSnapshot("form");

    assertEquals(List.of("# webforJ testbed snapshot v1",
        "- FlexLayout [name=login-form] [composite=Element] [node=div]:",
        "  - TextField \"Ada\" [name=name] [label=Name] [required]", "  - Button \"Save\"",
        "  - Button \"Cancel\" [hidden]",
        "  - \"Div \\\"Say \\\\\\\"hi\\\\\\\": now\\\" [composite=Element] [node=div]\"",
        "  - Element [node=section]"), readLines(snapshotFile("form")));
  }

  @Test
  void shouldLetTheApplicationDescribeItsOwnComponents() {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    CustomPicker picker = new CustomPicker("red", "green");
    picker.select("green");
    context.add(picker);

    verifyThat(picker).matchesSnapshot("picker");

    assertEquals("- CustomPicker [composite=Element] [node=x-picker] [selected=green]",
        readLines(snapshotFile("picker")).get(1));
  }

  @Test
  void shouldReportSnapshotFilesNoTestVerifies() {
    write(snapshotFile("kept"), "# webforJ testbed snapshot v1\n- Button\n");
    write(snapshotFile("stray"), "# webforJ testbed snapshot v1\n- Button\n");

    Optional<String> report = SnapshotStore.findObsolete(getClass(), Set.of("kept"));

    assertTrue(report.isPresent());
    assertTrue(report.get().contains("stray.yml"));
    assertFalse(report.get().contains("kept.yml"));
    assertTrue(SnapshotStore.findObsolete(getClass(), Set.of("kept", "stray")).isEmpty());
  }

  @Test
  void shouldLeaveTheSnapshotsOfNeighborClassesAlone() {
    Path neighbor = packageFolder().resolve("SnapshotTestNeighbor.stray.yml");
    write(neighbor, "# webforJ testbed snapshot v1\n- Button\n");
    System.setProperty(SnapshotStore.UPDATE_PROPERTY, "true");

    Optional<String> report = SnapshotStore.findObsolete(getClass(), Set.of());

    assertTrue(report.isEmpty());
    assertTrue(Files.exists(neighbor));
  }

  @Test
  void shouldDeleteObsoleteSnapshotFilesWhenUpdating() {
    write(snapshotFile("stray"), "# webforJ testbed snapshot v1\n- Button\n");
    System.setProperty(SnapshotStore.UPDATE_PROPERTY, "true");

    Optional<String> report = SnapshotStore.findObsolete(getClass(), Set.of());

    assertTrue(report.isEmpty());
    assertFalse(Files.exists(snapshotFile("stray")));
  }

  @Test
  void shouldReadTheCiModeFromTheEnvironmentUnlessTheGivenPropertyDecides() {
    System.setProperty(SnapshotStore.CI_PROPERTY, "true");
    assertTrue(SnapshotStore.isCi());

    System.setProperty(SnapshotStore.CI_PROPERTY, "false");
    assertFalse(SnapshotStore.isCi());

    System.clearProperty(SnapshotStore.CI_PROPERTY);
    String environment = System.getenv("CI");
    boolean expected = environment != null && !environment.isBlank()
        && !environment.equalsIgnoreCase("false") && !environment.equals("0");
    assertEquals(expected, SnapshotStore.isCi());
  }

  private Path snapshotFile(String name) {
    return packageFolder().resolve(getClass().getSimpleName() + "." + name + ".yml");
  }

  private Path packageFolder() {
    return snapshotDirectory.resolve(Path.of("com", "webforj", "testbed", "snapshot"));
  }

  private static void restore(String property, String value) {
    if (value == null) {
      System.clearProperty(property);
    } else {
      System.setProperty(property, value);
    }
  }

  private static List<String> readLines(Path file) {
    try {
      return Files.readAllLines(file);
    } catch (IOException e) {
      throw new IllegalStateException("Failed to read the snapshot file " + file, e);
    }
  }

  private static void write(Path file, String content) {
    try {
      Files.createDirectories(file.getParent());
      Files.writeString(file, content);
    } catch (IOException e) {
      throw new IllegalStateException("Failed to write the snapshot file " + file, e);
    }
  }
}
