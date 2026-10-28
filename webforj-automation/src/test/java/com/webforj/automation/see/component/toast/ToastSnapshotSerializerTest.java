package com.webforj.automation.see.component.toast;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.toast.Toast;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class ToastSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    Toast toast = new Toast("Saved");

    assertEquals("- Toast \"Saved\"\n",
        Snapshot.render(toast, new SnapshotOptions().setBrowserState(browserState)));
  }
}
