package com.webforj.testbed.snapshot;

import static com.webforj.testbed.assertion.WebforjAssertions.verifyThat;

import com.webforj.component.Component;
import com.webforj.testbed.TestbedTestSupport;
import com.webforj.testbed.WebforjTestbed;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

class FoundationSnapshotTest extends TestbedTestSupport {

  @ParameterizedTest(name = "{0}")
  @MethodSource("com.webforj.testbed.catalog.FoundationComponents#discover")
  void shouldMatchTheStoredSnapshot(Class<? extends Component> type) {
    WebforjTestbed context = WebforjTestbed.getCurrent();
    Component component = SnapshotFixtures.build(type, context).orElseThrow(
        () -> new AssertionError(type.getName() + " has no fixture, add one to SnapshotFixtures"));
    if (!component.isAttached()) {
      context.add(component);
    }

    verifyThat(component).matchesSnapshot(type.getSimpleName());
  }
}
