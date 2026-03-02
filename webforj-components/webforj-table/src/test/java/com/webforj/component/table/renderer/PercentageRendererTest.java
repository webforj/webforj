package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Theme;
import org.junit.jupiter.api.Test;

class PercentageRendererTest {

  @Test
  void shouldBuildDefault() {
    PercentageRenderer<String> r = new PercentageRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<dwc-progressbar"));
    assertTrue(html.contains("value='<%= cell.value %>'"));
    assertTrue(html.contains("text-visible='false'"));
    assertTrue(html.contains("--dwc-progressbar-height"));
    assertTrue(html.contains("<%= cell.value %>%"));
  }

  @Test
  void shouldBuildWithoutProgressBar() {
    PercentageRenderer<String> r = new PercentageRenderer<>();
    r.setShowProgressBar(false);
    String html = r.build();
    assertTrue(html.contains("<%= cell.value %>%"));
    assertTrue(html.contains("text-align:right"));
    assertTrue(html.contains("tabular-nums"));
    assertFalse(html.contains("dwc-progressbar"));
  }

  @Test
  void shouldBuildWithTheme() {
    PercentageRenderer<String> r = new PercentageRenderer<>(Theme.SUCCESS);
    assertTrue(r.build().contains("theme='success'"));
  }

  @Test
  void shouldNotBuildThemeWhenNotSet() {
    PercentageRenderer<String> r = new PercentageRenderer<>();
    assertFalse(r.build().contains("theme="));
  }
}
