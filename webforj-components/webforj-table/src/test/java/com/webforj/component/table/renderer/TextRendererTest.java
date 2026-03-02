package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Theme;
import com.webforj.component.table.renderer.TextRenderer.TextDecoration;
import java.util.EnumSet;
import java.util.Set;
import org.junit.jupiter.api.Test;

class TextRendererTest {

  @Test
  void shouldBuildDefault() {
    TextRenderer<String> r = new TextRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<span"));
    assertTrue(html.contains("</span>"));
    assertTrue(html.contains("<%= cell.value %>"));
    assertFalse(html.contains("style="));
  }

  @Test
  void shouldBuildWithContent() {
    TextRenderer<String> r = new TextRenderer<>("Hello");
    assertTrue(r.build().contains("Hello"));
  }

  @Test
  void shouldBuildWithTheme() {
    TextRenderer<String> r = new TextRenderer<>("Hello", Theme.PRIMARY);
    assertTrue(r.build().contains("color:var(--dwc-color-primary)"));
  }

  @Test
  void shouldBuildWithThemeOnly() {
    TextRenderer<String> r = new TextRenderer<>(Theme.SUCCESS);
    String html = r.build();
    assertTrue(html.contains("color:var(--dwc-color-success)"));
    assertTrue(html.contains("<%= cell.value %>"));
  }

  @Test
  void shouldBuildWithThemeAndShade() {
    TextRenderer<String> r = new TextRenderer<>();
    r.setTheme(Theme.SUCCESS);
    r.setShade(40);
    assertTrue(r.build().contains("color:var(--dwc-color-success-40)"));
  }

  @Test
  void shouldBuildWithDecorations() {
    TextRenderer<String> r = new TextRenderer<>();
    r.setDecorations(EnumSet.of(TextDecoration.BOLD, TextDecoration.ITALIC));
    String html = r.build();
    assertTrue(html.contains("font-weight:bold"));
    assertTrue(html.contains("font-style:italic"));
  }

  @Test
  void shouldBuildWithThemeAndDecorations() {
    TextRenderer<String> r = new TextRenderer<>();
    r.setTheme(Theme.PRIMARY);
    r.setDecorations(EnumSet.of(TextDecoration.BOLD));
    String html = r.build();
    assertTrue(html.contains("color:var(--dwc-color-primary)"));
    assertTrue(html.contains("font-weight:bold"));
  }

  @Test
  void shouldRemoveStyleWhenAllCleared() {
    TextRenderer<String> r = new TextRenderer<>();
    r.setTheme(Theme.PRIMARY);
    r.setDecorations(EnumSet.of(TextDecoration.BOLD));
    assertTrue(r.build().contains("style="));
    r.setTheme(null);
    r.setDecorations(Set.of());
    assertNull(r.getTheme());
    assertFalse(r.build().contains("style="));
  }
}
