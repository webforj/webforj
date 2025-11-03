package com.webforj.i18n.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Locale;
import org.junit.jupiter.api.Test;

class LocaleEventTest {

  @Test
  void shouldCreateLocaleEventWithSourceAndLocale() {
    Object source = new Object();
    Locale locale = Locale.FRENCH;

    LocaleEvent event = new LocaleEvent(source, locale);

    assertNotNull(event);
    assertEquals(source, event.getSource());
    assertEquals(locale, event.getLocale());
  }

  @Test
  void shouldReturnCorrectLocale() {
    Object source = "test-source";
    Locale locale = Locale.GERMAN;

    LocaleEvent event = new LocaleEvent(source, locale);

    assertEquals(locale, event.getLocale());
  }

  @Test
  void shouldReturnCorrectSource() {
    Object source = this;
    Locale locale = Locale.ENGLISH;

    LocaleEvent event = new LocaleEvent(source, locale);

    assertEquals(source, event.getSource());
  }
}
