package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class PhoneRendererTest {

  @Test
  void shouldBuildWithDefaultIcon() {
    PhoneRenderer<String> r = new PhoneRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<a"));
    assertTrue(html.contains("</a>"));
    assertTrue(html.contains("<%= 'tel:' + cell.value %>"));
    assertTrue(html.contains("tabindex='-1'"));
    assertTrue(html.contains("<dwc-icon name='phone' pool='tabler' theme='primary'></dwc-icon>"));
    assertTrue(html.contains("color:inherit;text-decoration:none"));
  }

  @Test
  void shouldBuildWithCustomIcon() {
    PhoneRenderer<String> r = new PhoneRenderer<>(new TestIconDefinition("phone-call", "custom"));
    String html = r.build();
    assertTrue(
        html.contains("<dwc-icon name='phone-call' pool='custom' theme='primary'></dwc-icon>"));
    assertTrue(html.contains("<%= 'tel:' + cell.value %>"));
    assertTrue(html.contains("tabindex='-1'"));
  }

  @Test
  void shouldBuildWithUpdatedIcon() {
    PhoneRenderer<String> r = new PhoneRenderer<>();
    r.setName("phone-call");
    r.setPool("custom");
    String html = r.build();
    assertTrue(
        html.contains("<dwc-icon name='phone-call' pool='custom' theme='primary'></dwc-icon>"));
  }

  @Test
  void shouldSetAndGetIcon() {
    PhoneRenderer<String> r = new PhoneRenderer<>();
    assertNotNull(r.getIcon());
    assertEquals("phone", r.getName());
    assertEquals("tabler", r.getPool());

    TestIconDefinition icon = new TestIconDefinition("device-mobile", "custom");
    r.setIcon(icon);
    assertEquals("device-mobile", r.getName());
    assertEquals("custom", r.getPool());
    assertEquals(icon, r.getIcon());
  }
}
