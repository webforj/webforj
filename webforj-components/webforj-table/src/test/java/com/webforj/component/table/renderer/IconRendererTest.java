package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Theme;
import com.webforj.component.icons.TablerIcon;
import org.junit.jupiter.api.Test;

class IconRendererTest {

  @Test
  void shouldBuildDefault() {
    IconRenderer<String> r = new IconRenderer<>(TablerIcon.create("heart"));
    String html = r.build();
    assertTrue(html.contains("<dwc-icon"));
    assertTrue(html.contains("name='heart'"));
    assertTrue(html.contains("pool='tabler'"));
  }

  @Test
  void shouldBuildWithCustomIconDefinition() {
    IconRenderer<String> r = new IconRenderer<>(new TestIconDefinition("star", "custom"));
    String html = r.build();
    assertTrue(html.contains("name='star'"));
    assertTrue(html.contains("pool='custom'"));
  }

  @Test
  void shouldSyncThemeFromIconDefinition() {
    IconRenderer<String> r = new IconRenderer<>(TablerIcon.create("check").setTheme(Theme.SUCCESS));
    assertTrue(r.build().contains("theme='success'"));
  }

  @Test
  void shouldNotSyncThemeForPlainIconDefinition() {
    IconRenderer<String> r = new IconRenderer<>(new TestIconDefinition("star", "custom"));
    assertFalse(r.build().contains("theme="));
  }

  @Test
  void shouldUpdateIconViaSetIcon() {
    IconRenderer<String> r = new IconRenderer<>(TablerIcon.create("old"));
    r.setIcon(TablerIcon.create("new-icon"));
    assertEquals("new-icon", r.getName());
    assertTrue(r.build().contains("name='new-icon'"));
  }
}
