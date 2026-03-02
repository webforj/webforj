package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class MaskedTextRendererTest {

  @Test
  void shouldBuildDefault() {
    MaskedTextRenderer<String> r = new MaskedTextRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<dwc-format-text"));
    assertTrue(html.contains("value='<%= cell.value %>'"));
    assertTrue(html.contains("mask='" + MaskedTextRenderer.DEFAULT_MASK + "'"));
  }

  @Test
  void shouldBuildWithMask() {
    MaskedTextRenderer<String> r = new MaskedTextRenderer<>("(###) ###-####");
    assertTrue(r.build().contains("mask='(###) ###-####'"));
  }

  @Test
  void shouldUpdateMask() {
    MaskedTextRenderer<String> r = new MaskedTextRenderer<>("##-###");
    r.setMask("XX-XXX-XXXX");
    assertEquals("XX-XXX-XXXX", r.getMask());
    assertTrue(r.build().contains("mask='XX-XXX-XXXX'"));
  }
}
