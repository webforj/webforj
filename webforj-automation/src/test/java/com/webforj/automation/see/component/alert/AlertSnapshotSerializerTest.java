package com.webforj.automation.see.component.alert;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.alert.Alert;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class AlertSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    Alert saved = new Alert("Saved");

    assertEquals("- Alert \"Saved\"\n",
        Snapshot.render(saved, new SnapshotOptions().setBrowserState(browserState)));
  }
}
