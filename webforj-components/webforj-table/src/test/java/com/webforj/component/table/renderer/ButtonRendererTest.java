package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.button.ButtonTheme;
import org.junit.jupiter.api.Test;

class ButtonRendererTest {

  @Test
  void shouldBuildDefault() {
    ButtonRenderer<String> r = new ButtonRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<dwc-button"));
    assertTrue(html.contains("</dwc-button>"));
    assertTrue(html.contains("<%= cell.value %>"));
    assertTrue(html.contains("expanse=''"));
    assertTrue(html.contains("tab-traversable='-1'"));
  }

  @Test
  void shouldBuildWithContent() {
    ButtonRenderer<String> r = new ButtonRenderer<>("Save");
    assertTrue(r.build().contains("Save"));
  }

  @Test
  void shouldBuildWithTheme() {
    ButtonRenderer<String> r = new ButtonRenderer<>();
    r.setTheme(ButtonTheme.PRIMARY);
    assertTrue(r.build().contains("theme='primary'"));
  }

  @Test
  void shouldBuildDisabled() {
    ButtonRenderer<String> r = new ButtonRenderer<>();
    r.setEnabled(false);
    assertTrue(r.build().contains("disabled='true'"));
  }

  @Test
  void shouldBuildEnabled() {
    ButtonRenderer<String> r = new ButtonRenderer<>();
    r.setEnabled(false);
    r.setEnabled(true);
    assertFalse(r.build().contains("disabled="));
  }
}
