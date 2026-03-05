package com.webforj.component.table.renderer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Locale;
import org.junit.jupiter.api.Test;

class MaskedDateTimeRendererTest {

  @Test
  void shouldBuildDefault() {
    MaskedDateTimeRenderer<String> r = new MaskedDateTimeRenderer<>();
    String html = r.build();
    assertTrue(html.contains("<dwc-format-datetime"));
    assertTrue(html.contains("value='<%= cell.value %>'"));
    assertTrue(html.contains("mask='" + MaskedDateTimeRenderer.DEFAULT_MASK + "'"));
    assertTrue(html.contains("locale="));
  }

  @Test
  void shouldBuildWithCustomMask() {
    MaskedDateTimeRenderer<String> r = new MaskedDateTimeRenderer<>("%Yd-%Mz-%Dz");
    assertTrue(r.build().contains("mask='%Yd-%Mz-%Dz'"));
  }

  @Test
  void shouldBuildWithTimeMask() {
    MaskedDateTimeRenderer<String> r = new MaskedDateTimeRenderer<>("%Hz:%mz");
    assertTrue(r.build().contains("mask='%Hz:%mz'"));
  }

  @Test
  void shouldBuildWithLocale() {
    MaskedDateTimeRenderer<String> r = new MaskedDateTimeRenderer<>("%Mz/%Dz/%Yl", Locale.GERMANY);
    String html = r.build();
    assertTrue(html.contains("locale='de-DE'"));
    assertEquals(Locale.GERMANY, r.getLocale());
  }

  @Test
  void shouldUpdateLocale() {
    MaskedDateTimeRenderer<String> r = new MaskedDateTimeRenderer<>();
    r.setLocale(Locale.FRANCE);
    assertTrue(r.build().contains("locale='fr-FR'"));
  }

  @Test
  void shouldUpdateMask() {
    MaskedDateTimeRenderer<String> r = new MaskedDateTimeRenderer<>("%Mz/%Dz/%Yl");
    r.setMask("%Hz:%mz:%sz");
    assertEquals("%Hz:%mz:%sz", r.getMask());
    assertTrue(r.build().contains("mask='%Hz:%mz:%sz'"));
  }
}
