package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Currency;
import java.util.Locale;
import org.junit.jupiter.api.Test;

class CurrencyRendererTest {

  @Test
  void shouldBuildWithUSLocale() {
    CurrencyRenderer<String> r = new CurrencyRenderer<>(Locale.US);
    String html = r.build();
    assertTrue(html.contains("window.__cfmt_" + r.getKey()));
    assertTrue(html.contains("Intl.NumberFormat('en-US'"));
    assertTrue(html.contains("currency: 'USD'"));
    assertTrue(html.contains(".format(cell.value)"));
    assertTrue(html.contains("tabular-nums"));
    assertTrue(html.contains("text-align:right"));
  }

  @Test
  void shouldBuildWithGermanLocale() {
    CurrencyRenderer<String> r = new CurrencyRenderer<>(Locale.GERMANY);
    String html = r.build();
    assertTrue(html.contains("Intl.NumberFormat('de-DE'"));
    assertTrue(html.contains("currency: 'EUR'"));
  }

  @Test
  void shouldBuildWithCurrencyOverride() {
    CurrencyRenderer<String> r = new CurrencyRenderer<>(Locale.US, Currency.getInstance("EUR"));
    String html = r.build();
    assertTrue(html.contains("Intl.NumberFormat('en-US'"));
    assertTrue(html.contains("currency: 'EUR'"));
  }

  @Test
  void shouldSetLocale() {
    CurrencyRenderer<String> r = new CurrencyRenderer<>(Locale.US);
    r.setLocale(Locale.FRANCE);
    assertEquals(Locale.FRANCE, r.getLocale());
    assertEquals("EUR", r.getCurrency().getCurrencyCode());
    assertTrue(r.build().contains("Intl.NumberFormat('fr-FR'"));
  }

  @Test
  void shouldSetCurrency() {
    CurrencyRenderer<String> r = new CurrencyRenderer<>(Locale.US);
    r.setCurrency(Currency.getInstance("GBP"));
    assertEquals("GBP", r.getCurrency().getCurrencyCode());
    assertTrue(r.build().contains("currency: 'GBP'"));
  }

  @Test
  void shouldCacheFormatterOnWindow() {
    CurrencyRenderer<String> r = new CurrencyRenderer<>(Locale.US);
    String html = r.build();
    String key = "__cfmt_" + r.getKey();
    assertTrue(html.contains("if (!window." + key + ")"));
    assertTrue(html.contains("window." + key + " = new Intl.NumberFormat"));
    assertTrue(html.contains("window." + key + ".format(cell.value)"));
  }
}
