package com.webforj.automation.see.component.html;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.automation.see.Snapshot;
import com.webforj.component.html.elements.Img;
import org.junit.jupiter.api.Test;

class ImgSnapshotSerializerTest {

  @Test
  void shouldPrintTheSourceAndTheAlternativeTextByDefault() {
    Img logo = new Img("logo.png", "Company logo");

    assertEquals("- Img [src=logo.png] [alt=\"Company logo\"]\n", Snapshot.render(logo));
  }
}
