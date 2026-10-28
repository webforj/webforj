package com.webforj.automation.see.component.html;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.component.html.elements.Iframe;
import org.junit.jupiter.api.Test;

class IframeSnapshotSerializerTest {

  @Test
  void shouldPrintTheSourceByDefault() {
    Iframe frame = new Iframe();
    frame.setSrc("https://webforj.com");

    assertEquals("- Iframe [src=\"https://webforj.com\"]\n", Snapshot.render(frame));
  }
}
