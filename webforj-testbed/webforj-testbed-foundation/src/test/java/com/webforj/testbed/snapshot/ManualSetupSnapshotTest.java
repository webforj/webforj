package com.webforj.testbed.snapshot;

import static com.webforj.testbed.assertion.WebforjAssertions.verifyThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.button.Button;
import com.webforj.testbed.WebforjTestbed;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ManualSetupSnapshotTest {
  @TempDir
  Path snapshotDirectory;
  private String previousDirectory;
  private String previousCi;

  @BeforeEach
  void setUpByHand() {
    previousDirectory = System.getProperty(SnapshotStore.DIRECTORY_PROPERTY);
    previousCi = System.getProperty(SnapshotStore.CI_PROPERTY);
    System.setProperty(SnapshotStore.DIRECTORY_PROPERTY, snapshotDirectory.toString());
    System.setProperty(SnapshotStore.CI_PROPERTY, "false");
    WebforjTestbed.setup();
  }

  @AfterEach
  void tearDownByHand() {
    WebforjTestbed.tearDown();
    restore(SnapshotStore.DIRECTORY_PROPERTY, previousDirectory);
    restore(SnapshotStore.CI_PROPERTY, previousCi);
  }

  @Test
  void shouldNameTheSnapshotAfterTheCallingTestMethodWithoutTheExtension() {
    Button button = new Button("Save");
    WebforjTestbed.getCurrent().add(button);

    verifyThat(button).matchesSnapshot();

    assertTrue(Files.exists(snapshotDirectory
        .resolve(Path.of("com", "webforj", "testbed", "snapshot", "ManualSetupSnapshotTest."
            + "shouldNameTheSnapshotAfterTheCallingTestMethodWithoutTheExtension.yml"))));
  }

  private static void restore(String property, String value) {
    if (value == null) {
      System.clearProperty(property);
    } else {
      System.setProperty(property, value);
    }
  }
}
