package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Theme;
import com.webforj.component.icons.TablerIcon;
import org.junit.jupiter.api.Test;

class BooleanRendererTest {

  @Test
  void shouldBuildDefault() {
    BooleanRenderer<String> r = new BooleanRenderer<>();
    String html = r.build();
    assertTrue(html.contains("cell.value === true || cell.value === 'true'"));
    assertTrue(html.contains("cell.value === false || cell.value === 'false'"));
    assertTrue(html.contains("name='circle-check'"));
    assertTrue(html.contains("name='circle-x'"));
    assertTrue(html.contains("theme='success'"));
    assertTrue(html.contains("theme='danger'"));
  }

  @Test
  void shouldNotBuildNullBranchByDefault() {
    BooleanRenderer<String> r = new BooleanRenderer<>();
    assertFalse(r.build().contains("circle-minus"));
  }

  @Test
  void shouldBuildWithShowNull() {
    BooleanRenderer<String> r = new BooleanRenderer<>();
    r.setShowNull(true);
    assertTrue(r.build().contains("name='circle-minus'"));
  }

  @Test
  void shouldBuildWithCustomIcons() {
    BooleanRenderer<String> r =
        new BooleanRenderer<>(TablerIcon.create("eye").setTheme(Theme.SUCCESS),
            TablerIcon.create("eye-off").setTheme(Theme.DANGER));
    String html = r.build();
    assertTrue(html.contains("name='eye'"));
    assertTrue(html.contains("name='eye-off'"));
  }

  @Test
  void shouldNotBuildThemeForPlainIconDefinition() {
    BooleanRenderer<String> r = new BooleanRenderer<>();
    r.setTrueIcon(new TestIconDefinition("check", "custom"));
    assertTrue(r.build().contains("name='check' pool='custom'></dwc-icon>"));
  }
}
