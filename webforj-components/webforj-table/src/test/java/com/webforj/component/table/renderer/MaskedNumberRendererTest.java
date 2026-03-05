package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.text.DecimalFormatSymbols;
import java.util.Locale;
import org.junit.jupiter.api.Test;

class MaskedNumberRendererTest {

  @Test
  void shouldBuildDefault() {
    MaskedNumberRenderer<String> r = new MaskedNumberRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<dwc-format-number"));
    assertTrue(html.contains("value='<%= cell.value %>'"));
    assertTrue(html.contains("mask='" + MaskedNumberRenderer.DEFAULT_MASK + "'"));
  }

  @Test
  void shouldBuildWithMask() {
    MaskedNumberRenderer<String> r = new MaskedNumberRenderer<>("###,##0.00");
    assertTrue(r.build().contains("mask='###,##0.00'"));
  }

  @Test
  void shouldDeriveSeparatorsFromLocale() {
    MaskedNumberRenderer<String> r = new MaskedNumberRenderer<>("###,##0.00", Locale.GERMANY);
    DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.GERMANY);
    String html = r.build();
    assertTrue(html.contains("group-separator='" + symbols.getGroupingSeparator() + "'"));
    assertTrue(html.contains("decimal-separator='" + symbols.getDecimalSeparator() + "'"));
  }

  @Test
  void shouldUpdateLocale() {
    MaskedNumberRenderer<String> r = new MaskedNumberRenderer<>();
    r.setLocale(Locale.FRANCE);
    assertEquals(Locale.FRANCE, r.getLocale());
    DecimalFormatSymbols symbols = new DecimalFormatSymbols(Locale.FRANCE);
    String html = r.build();
    assertTrue(html.contains("decimal-separator='" + symbols.getDecimalSeparator() + "'"));
  }

  @Test
  void shouldUpdateMask() {
    MaskedNumberRenderer<String> r = new MaskedNumberRenderer<>("##0");
    r.setMask("###,##0.00");
    assertEquals("###,##0.00", r.getMask());
    assertTrue(r.build().contains("mask='###,##0.00'"));
  }
}
