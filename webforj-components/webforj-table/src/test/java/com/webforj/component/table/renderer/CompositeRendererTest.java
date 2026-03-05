package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.layout.flexlayout.FlexAlignment;
import com.webforj.component.layout.flexlayout.FlexDirection;
import com.webforj.component.layout.flexlayout.FlexJustifyContent;
import com.webforj.component.layout.flexlayout.FlexWrap;
import org.junit.jupiter.api.Test;

class CompositeRendererTest {

  @Test
  void shouldBuildWithDefaultFlexStyles() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    String html = r.build();
    assertTrue(html.contains("display:flex"));
    assertTrue(html.contains("flex-direction:row"));
    assertTrue(html.contains("align-items:center"));
    assertTrue(html.contains("gap:8px"));
  }

  @Test
  void shouldCombineSubRenderers() {
    TextRenderer<String> text = new TextRenderer<>();
    NullRenderer<String> nullR = new NullRenderer<>();
    CompositeRenderer<String> r = new CompositeRenderer<>(text, nullR);
    String html = r.build();
    assertTrue(html.contains(text.build()));
    assertTrue(html.contains(nullR.build()));
  }

  @Test
  void shouldAddSubRenderer() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    TextRenderer<String> text = new TextRenderer<>();
    r.add(text);
    assertTrue(r.build().contains(text.build()));
  }

  @Test
  void shouldSetAndGetSpacing() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    r.setSpacing("16px");
    assertEquals("16px", r.getSpacing());
    assertTrue(r.build().contains("gap:16px"));
  }

  @Test
  void shouldSetAndGetDirection() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    r.setDirection(FlexDirection.COLUMN);
    assertEquals(FlexDirection.COLUMN, r.getDirection());
    assertTrue(r.build().contains("flex-direction:column"));
  }

  @Test
  void shouldSetAndGetAlignment() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    r.setAlignment(FlexAlignment.START);
    assertEquals(FlexAlignment.START, r.getAlignment());
    assertTrue(r.build().contains("align-items:flex-start"));
  }

  @Test
  void shouldSetAndGetJustifyContent() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    assertNull(r.getJustifyContent());
    r.setJustifyContent(FlexJustifyContent.BETWEEN);
    assertEquals(FlexJustifyContent.BETWEEN, r.getJustifyContent());
    assertTrue(r.build().contains("justify-content:space-between"));
  }

  @Test
  void shouldNotIncludeJustifyContentWhenNull() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    String html = r.build();
    assertFalse(html.contains("justify-content"));
  }

  @Test
  void shouldSetAndGetWrap() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    assertNull(r.getWrap());
    r.setWrap(FlexWrap.WRAP);
    assertEquals(FlexWrap.WRAP, r.getWrap());
    assertTrue(r.build().contains("flex-wrap:wrap"));
  }

  @Test
  void shouldNotIncludeWrapWhenNull() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    String html = r.build();
    assertFalse(html.contains("flex-wrap"));
  }

  @Test
  void shouldWrapOutputInSpan() {
    CompositeRenderer<String> r = new CompositeRenderer<>();
    String html = r.build();
    assertTrue(html.startsWith("<span style='"));
    assertTrue(html.endsWith("</span>"));
  }
}
