package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class ImageRendererTest {

  @Test
  void shouldBuildDefault() {
    ImageRenderer<String> r = new ImageRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<img"));
    assertTrue(html.contains("src='<%= cell.value %>'"));
    assertTrue(html.contains("display:block"));
    assertTrue(html.contains("vertical-align:middle"));
    assertTrue(html.contains("max-height:100%"));
  }

  @Test
  void shouldBuildWithSrc() {
    ImageRenderer<String> r = new ImageRenderer<>("https://example.com/img.png");
    String html = r.build();
    assertTrue(html.contains("src='https://example.com/img.png'"));
    assertFalse(html.contains("<%= cell.value %>"));
  }

  @Test
  void shouldBuildWithSrcAndAlt() {
    ImageRenderer<String> r = new ImageRenderer<>("https://example.com/img.png", "Logo");
    String html = r.build();
    assertTrue(html.contains("src='https://example.com/img.png'"));
    assertTrue(html.contains("alt='Logo'"));
  }

  @Test
  void shouldBuildWithDimensions() {
    ImageRenderer<String> r = new ImageRenderer<>();
    r.setWidth("40px").setHeight("40px");
    String html = r.build();
    assertTrue(html.contains("width:40px"));
    assertTrue(html.contains("height:40px"));
    assertTrue(html.contains("display:block"));
    assertTrue(html.contains("max-height:100%"));
  }

  @Test
  void shouldBindSrcToCellValueWhenNotSet() {
    ImageRenderer<String> r = new ImageRenderer<>();
    r.setAlt("Thumbnail");
    String html = r.build();
    assertTrue(html.contains("src='<%= cell.value %>'"));
    assertTrue(html.contains("alt='Thumbnail'"));
  }

  @Test
  void shouldBuildWithTemplateExpressionInSrc() {
    ImageRenderer<String> r = new ImageRenderer<>();
    r.setSrc("https://placehold.co/40x40?text=<%= cell.value %>");
    String html = r.build();
    assertTrue(html.contains("src='https://placehold.co/40x40?text=<%= cell.value %>'"));
  }
}
