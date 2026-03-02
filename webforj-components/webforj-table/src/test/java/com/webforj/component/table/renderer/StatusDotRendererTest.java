package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Theme;
import java.awt.Color;
import org.junit.jupiter.api.Test;

class StatusDotRendererTest {

  @Test
  void shouldBuildDefault() {
    StatusDotRenderer<String> r = new StatusDotRenderer<>();
    String html = r.build();
    assertTrue(html.contains("var(--dwc-color-default)"));
    assertTrue(html.contains("<%= cell.value %>"));
  }

  @Test
  void shouldBuildWithMapping() {
    StatusDotRenderer<String> r = new StatusDotRenderer<>();
    r.addMapping("active", "green");
    String html = r.build();
    assertTrue(html.contains("active"));
    assertTrue(html.contains("green"));
  }

  @Test
  void shouldRemoveMapping() {
    StatusDotRenderer<String> r = new StatusDotRenderer<>();
    r.addMapping("active", "green");
    r.removeMapping("active");
    assertTrue(r.getColorMap().isEmpty());
  }

  @Test
  void shouldBuildWithCustomDefaultColorAndSize() {
    StatusDotRenderer<String> r = new StatusDotRenderer<>();
    r.setDefaultColor("#ccc");
    r.setDotSize("12px");
    String html = r.build();
    assertTrue(html.contains("#ccc"));
    assertTrue(html.contains("width:12px"));
    assertTrue(html.contains("height:12px"));
  }

  @Test
  void shouldEscapeSingleQuotesInKeys() {
    StatusDotRenderer<String> r = new StatusDotRenderer<>();
    r.addMapping("it's active", "green");
    assertTrue(r.build().contains("it\\'s active"));
  }

  @Test
  void shouldAcceptColorAndThemeOverloads() {
    StatusDotRenderer<String> r = new StatusDotRenderer<>();
    r.addMapping("active", Color.RED);
    assertEquals("#ff0000", r.getColorMap().get("active"));

    r.addMapping("pending", Theme.SUCCESS);
    assertEquals("var(--dwc-color-success)", r.getColorMap().get("pending"));

    r.setDefaultColor(Color.BLUE);
    assertEquals("#0000ff", r.getDefaultColor());

    r.setDefaultColor(Theme.DANGER);
    assertEquals("var(--dwc-color-danger)", r.getDefaultColor());
  }
}
