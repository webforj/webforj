package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class AnchorRendererTest {

  @Test
  void shouldBuildDefault() {
    AnchorRenderer<String> r = new AnchorRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<a"));
    assertTrue(html.contains("</a>"));
    assertTrue(html.contains("<%= cell.value %>"));
    assertTrue(html.contains("tabindex='-1'"));
  }

  @Test
  void shouldBuildWithConstructorArgs() {
    AnchorRenderer<String> r = new AnchorRenderer<>("https://example.com", "Click here", "_blank");
    String html = r.build();
    assertTrue(html.contains("href='https://example.com'"));
    assertTrue(html.contains("target='_blank'"));
    assertTrue(html.contains("Click here"));
  }

  @Test
  void shouldBuildWithUrlAlias() {
    AnchorRenderer<String> r = new AnchorRenderer<>();
    r.setUrl("https://example.com");
    assertEquals("https://example.com", r.getUrl());
    assertTrue(r.build().contains("href='https://example.com'"));
  }

  @Test
  void shouldBuildWithTarget() {
    AnchorRenderer<String> r = new AnchorRenderer<>();
    r.setTarget("_blank");
    assertTrue(r.build().contains("target='_blank'"));
  }

  @Test
  void shouldBuildWithDownload() {
    AnchorRenderer<String> r = new AnchorRenderer<>();
    r.setDownload("doc.pdf");
    assertTrue(r.build().contains("download='doc.pdf'"));
  }
}
