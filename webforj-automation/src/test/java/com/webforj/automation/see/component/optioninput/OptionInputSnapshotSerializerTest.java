package com.webforj.automation.see.component.optioninput;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.optioninput.CheckBox;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class OptionInputSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    CheckBox gift = new CheckBox("Gift wrap");

    assertEquals("- CheckBox \"Gift wrap\"\n",
        Snapshot.render(gift, new SnapshotOptions().setBrowserState(browserState)));
  }
}
