package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Theme;
import com.webforj.component.icons.TablerIcon;
import org.junit.jupiter.api.Test;

class IconButtonRendererTest {

  @Test
  void shouldBuildDefault() {
    IconButtonRenderer<String> r = new IconButtonRenderer<>(TablerIcon.create("edit"));
    String html = r.build();
    assertTrue(html.contains("<dwc-icon-button"));
    assertTrue(html.contains("name='edit'"));
    assertTrue(html.contains("pool='tabler'"));
    assertTrue(html.contains("expanse=''"));
    assertTrue(html.contains("tab-traversable='-1'"));
  }

  @Test
  void shouldBuildWithTheme() {
    IconButtonRenderer<String> r = new IconButtonRenderer<>(TablerIcon.create("edit"));
    r.setTheme(Theme.DANGER);
    assertTrue(r.build().contains("theme='danger'"));
  }

  @Test
  void shouldBuildDisabled() {
    IconButtonRenderer<String> r = new IconButtonRenderer<>(TablerIcon.create("edit"));
    r.setEnabled(false);
    assertFalse(r.isEnabled());
    assertTrue(r.build().contains("disabled='true'"));
  }

  @Test
  void shouldSyncThemeFromIconDefinition() {
    IconButtonRenderer<String> r =
        new IconButtonRenderer<>(TablerIcon.create("check").setTheme(Theme.SUCCESS));
    assertEquals(Theme.SUCCESS, r.getTheme());
    assertTrue(r.build().contains("theme='success'"));
  }

  @Test
  void shouldNotSyncThemeForPlainIconDefinition() {
    IconButtonRenderer<String> r =
        new IconButtonRenderer<>(new TestIconDefinition("star", "custom"));
    assertFalse(r.build().contains("theme="));
  }

  @Test
  void shouldUpdateIconViaSetIcon() {
    IconButtonRenderer<String> r = new IconButtonRenderer<>(TablerIcon.create("edit"));
    r.setIcon(TablerIcon.create("trash"));
    assertEquals("trash", r.getName());
    assertTrue(r.build().contains("name='trash'"));
  }
}
