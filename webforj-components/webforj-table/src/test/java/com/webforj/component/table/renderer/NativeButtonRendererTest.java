package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class NativeButtonRendererTest {

  @Test
  void shouldBuildDefault() {
    NativeButtonRenderer<String> r = new NativeButtonRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<button"));
    assertTrue(html.contains("</button>"));
    assertTrue(html.contains("<%= cell.value %>"));
    assertTrue(html.contains("tabindex='-1'"));
  }

  @Test
  void shouldBuildWithContent() {
    NativeButtonRenderer<String> r = new NativeButtonRenderer<>("Click me");
    assertTrue(r.build().contains("Click me"));
  }

  @Test
  void shouldBuildDisabled() {
    NativeButtonRenderer<String> r = new NativeButtonRenderer<>();
    r.setEnabled(false);
    assertFalse(r.isEnabled());
    assertTrue(r.build().contains("disabled='true'"));
  }

  @Test
  void shouldBuildEnabled() {
    NativeButtonRenderer<String> r = new NativeButtonRenderer<>();
    r.setEnabled(false);
    r.setEnabled(true);
    assertTrue(r.isEnabled());
    assertFalse(r.build().contains("disabled="));
  }
}
