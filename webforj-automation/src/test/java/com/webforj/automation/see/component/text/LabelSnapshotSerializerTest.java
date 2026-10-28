package com.webforj.automation.see.component.text;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.text.Label;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class LabelSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    Label component = new Label("Note");

    assertEquals("- Label \"Note\"\n",
        Snapshot.render(component, new SnapshotOptions().setBrowserState(browserState)));
  }
}
