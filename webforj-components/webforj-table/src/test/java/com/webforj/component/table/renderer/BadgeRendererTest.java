package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.badge.BadgeTheme;
import java.awt.Color;
import org.junit.jupiter.api.Test;

class BadgeRendererTest {

  @Test
  void shouldBuildDefault() {
    BadgeRenderer<String> r = new BadgeRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<dwc-badge"));
    assertTrue(html.contains("</dwc-badge>"));
    assertTrue(html.contains("<%= cell.value %>"));
    assertTrue(html.contains("expanse=''"));
  }

  @Test
  void shouldBuildWithContentAndTheme() {
    BadgeRenderer<String> r = new BadgeRenderer<>("Active", BadgeTheme.SUCCESS);
    String html = r.build();
    assertTrue(html.contains("Active"));
    assertTrue(html.contains("theme='success'"));
  }

  @Test
  void shouldBuildWithColor() {
    BadgeRenderer<String> r = new BadgeRenderer<>();
    r.setColor(new Color(255, 87, 51));
    assertTrue(r.build().contains("style='--dwc-badge-seed:#ff5733'"));
  }

  @Test
  void shouldRemoveColorWhenNull() {
    BadgeRenderer<String> r = new BadgeRenderer<>();
    r.setColor(new Color(255, 87, 51));
    r.setColor(null);
    assertNull(r.getColor());
    assertFalse(r.build().contains("style="));
  }

  @Test
  void shouldBuildWithIcon() {
    BadgeRenderer<String> r = new BadgeRenderer<>("Active");
    r.setIcon(new TestIconDefinition("edit", "tabler"));
    String html = r.build();
    assertTrue(html.contains("<dwc-icon name='edit' pool='tabler'></dwc-icon>"));
    assertTrue(html.contains("Active"));
  }

  @Test
  void shouldNotBuildIconWhenNull() {
    BadgeRenderer<String> r = new BadgeRenderer<>("Active");
    r.setIcon(new TestIconDefinition("edit", "tabler"));
    r.setIcon(null);
    assertNull(r.getIcon());
    assertFalse(r.build().contains("dwc-icon"));
  }
}
