package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Theme;
import org.junit.jupiter.api.Test;

class ProgressBarRendererTest {

  @Test
  void shouldBuildDefault() {
    ProgressBarRenderer<String> r = new ProgressBarRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<dwc-progressbar"));
    assertTrue(html.contains("value='<%= cell.value %>'"));
    assertTrue(html.contains("--dwc-progressbar-height"));
    assertTrue(html.contains("--dwc-font-size-s"));
  }

  @Test
  void shouldBuildWithValue() {
    ProgressBarRenderer<String> r = new ProgressBarRenderer<>(50);
    String html = r.build();
    assertTrue(html.contains("value='50'"));
    assertFalse(html.contains("<%= cell.value %>"));
  }

  @Test
  void shouldBuildWithMinAndMax() {
    ProgressBarRenderer<String> r = new ProgressBarRenderer<>(50, 10, 200);
    String html = r.build();
    assertTrue(html.contains("min='10'"));
    assertTrue(html.contains("max='200'"));
  }

  @Test
  void shouldBuildWithTheme() {
    ProgressBarRenderer<String> r = new ProgressBarRenderer<>();
    r.setTheme(Theme.SUCCESS);
    assertTrue(r.build().contains("theme='success'"));
  }

  @Test
  void shouldBuildWithStripedAndAnimated() {
    ProgressBarRenderer<String> r = new ProgressBarRenderer<>();
    r.setStriped(true);
    r.setAnimated(true);
    String html = r.build();
    assertTrue(html.contains("striped='true'"));
    assertTrue(html.contains("animated='true'"));
  }

  @Test
  void shouldBuildWithTextAndIndeterminate() {
    ProgressBarRenderer<String> r = new ProgressBarRenderer<>();
    r.setText("{{x}}% done");
    r.setIndeterminate(true);
    String html = r.build();
    assertTrue(html.contains("text='{{x}}% done'"));
    assertTrue(html.contains("indeterminate='true'"));
  }
}
