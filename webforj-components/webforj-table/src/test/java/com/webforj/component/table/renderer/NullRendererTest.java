package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class NullRendererTest {

  @Test
  void shouldBuildDefault() {
    NullRenderer<String> r = new NullRenderer<>();
    String html = r.build();
    assertTrue(html.contains(NullRenderer.DEFAULT_PLACEHOLDER));
    assertTrue(html.contains("<%= cell.value %>"));
    assertTrue(html.contains("cell.value == null"));
    assertTrue(html.contains("cell.value === ''"));
    assertTrue(html.contains("cell.value === undefined"));
    assertTrue(html.contains("cell.value === 'null'"));
    assertTrue(html.contains("<span>"));
  }

  @Test
  void shouldBuildWithCustomPlaceholder() {
    NullRenderer<String> r = new NullRenderer<>("N/A");
    assertTrue(r.build().contains("N/A"));
  }

  @Test
  void shouldEscapeSingleQuotesInPlaceholder() {
    NullRenderer<String> r = new NullRenderer<>("it's null");
    assertTrue(r.build().contains("it\\'s null"));
  }
}
