package com.webforj.automation.see.component.window;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.window.Panel;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class PanelSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    Panel sidebar = new Panel();
    sidebar.setText("Menu");

    assertEquals("- Panel \"Menu\"\n",
        Snapshot.render(sidebar, new SnapshotOptions().setBrowserState(browserState)));
  }
}
