package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class EmailRendererTest {

  @Test
  void shouldBuildWithDefaultIcon() {
    EmailRenderer<String> r = new EmailRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<a"));
    assertTrue(html.contains("</a>"));
    assertTrue(html.contains("<%= 'mailto:' + cell.value %>"));
    assertTrue(html.contains("tabindex='-1'"));
    assertTrue(html.contains("<dwc-icon name='mail' pool='tabler' theme='primary'></dwc-icon>"));
    assertTrue(html.contains("color:inherit;text-decoration:none"));
  }

  @Test
  void shouldBuildWithCustomIcon() {
    EmailRenderer<String> r = new EmailRenderer<>(new TestIconDefinition("envelope", "custom"));
    String html = r.build();
    assertTrue(
        html.contains("<dwc-icon name='envelope' pool='custom' theme='primary'></dwc-icon>"));
    assertTrue(html.contains("<%= 'mailto:' + cell.value %>"));
    assertTrue(html.contains("tabindex='-1'"));
  }

  @Test
  void shouldBuildWithUpdatedIcon() {
    EmailRenderer<String> r = new EmailRenderer<>();
    r.setName("envelope");
    r.setPool("custom");
    String html = r.build();
    assertTrue(
        html.contains("<dwc-icon name='envelope' pool='custom' theme='primary'></dwc-icon>"));
  }

  @Test
  void shouldSetAndGetIcon() {
    EmailRenderer<String> r = new EmailRenderer<>();
    assertNotNull(r.getIcon());
    assertEquals("mail", r.getName());
    assertEquals("tabler", r.getPool());

    TestIconDefinition icon = new TestIconDefinition("at", "custom");
    r.setIcon(icon);
    assertEquals("at", r.getName());
    assertEquals("custom", r.getPool());
    assertEquals(icon, r.getIcon());
  }
}
