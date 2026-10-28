package com.webforj.automation.see.component.loading;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.loading.Loading;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class LoadingSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    Loading loading = new Loading("Please wait");

    assertEquals("- Loading \"Please wait\"\n",
        Snapshot.render(loading, new SnapshotOptions().setBrowserState(browserState)));
  }
}
