package com.webforj.automation.see.component.html;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.component.html.elements.Anchor;
import org.junit.jupiter.api.Test;

class AnchorSnapshotSerializerTest {

  @Test
  void shouldPrintTheLinkTargetByDefault() {
    Anchor anchor = new Anchor("https://webforj.com/docs", "Docs");

    assertEquals("- Anchor [href=\"https://webforj.com/docs\"]\n", Snapshot.render(anchor));
  }
}
