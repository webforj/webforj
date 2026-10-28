package com.webforj.testbed;

import com.webforj.testbed.snapshot.SnapshotStore;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.TestInfo;

/** Sets up the foundation tests through the framework-independent API. */
public abstract class TestbedTestSupport {

  @BeforeEach
  void setUpTestbed(TestInfo test) {
    WebforjTestbed.setup(options());
    SnapshotStore.beginTest(test.getTestClass().orElseThrow(),
        test.getTestMethod().orElseThrow().getName());
  }

  @AfterEach
  void tearDownTestbed() {
    try {
      SnapshotStore.endTest();
    } finally {
      WebforjTestbed.tearDown();
    }
  }

  /** Returns the runtime configuration for this test. */
  protected WebforjTestbedOptions options() {
    return new WebforjTestbedOptions();
  }
}
