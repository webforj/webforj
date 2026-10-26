package com.webforj.automation.see.component.markdown;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.automation.see.SnapshotOptions;
import com.webforj.component.markdown.MarkdownViewer;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class MarkdownViewerSnapshotSerializerTest {

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  void shouldPrintTheTextAtBothLevels(boolean browserState) {
    MarkdownViewer component = new MarkdownViewer("Note");

    assertEquals("- MarkdownViewer \"Note\"\n",
        Snapshot.render(component, new SnapshotOptions().setBrowserState(browserState)));
  }
}
