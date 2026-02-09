package com.webforj.i18n;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mockStatic;

import com.webforj.App;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

class BundleTranslationResolverTest {

  private BundleTranslationResolver resolver;

  @BeforeEach
  void setUp() {
    List<Locale> locales = Arrays.asList(Locale.ENGLISH, Locale.FRENCH, Locale.GERMAN);
    resolver = new BundleTranslationResolver(locales);
  }

  @Nested
  class Constructors {

    @Test
    void shouldUseDefaultBundleName() {
      BundleTranslationResolver custom = new BundleTranslationResolver(Collections.emptyList());
      assertEquals("messages", custom.getBundleName());
    }

    @Test
    void shouldAcceptCustomBundleName() {
      BundleTranslationResolver custom =
          new BundleTranslationResolver("custom", Collections.emptyList());
      assertEquals("custom", custom.getBundleName());
    }
  }

  @Nested
  class SupportedLocales {

    @Test
    void shouldReturnConfiguredLocales() {
      List<Locale> locales = resolver.getSupportedLocales();

      assertNotNull(locales);
      assertEquals(3, locales.size());
      assertTrue(locales.contains(Locale.ENGLISH));
      assertTrue(locales.contains(Locale.FRENCH));
      assertTrue(locales.contains(Locale.GERMAN));
    }

    @Test
    void shouldReturnFirstLocaleAsDefault() {
      assertEquals(Locale.ENGLISH, resolver.getDefaultLocale());
    }

    @Test
    void shouldReturnAppLocaleWhenNoLocalesConfigured() {
      BundleTranslationResolver empty = new BundleTranslationResolver(Collections.emptyList());

      try (MockedStatic<App> appMock = mockStatic(App.class)) {
        appMock.when(App::getLocale).thenReturn(Locale.ITALIAN);

        assertEquals(Locale.ITALIAN, empty.getDefaultLocale());
      }
    }
  }

  @Nested
  class SingleKeyResolution {

    @Test
    void shouldResolveKey() {
      assertEquals("Welcome", resolver.resolve("welcome", Locale.ENGLISH));
    }

    @Test
    void shouldResolveKeyForDifferentLocales() {
      assertEquals("Bienvenue", resolver.resolve("welcome", Locale.FRENCH));
      assertEquals("Willkommen", resolver.resolve("welcome", Locale.GERMAN));
    }

    @Test
    void shouldResolveKeyWithParameters() {
      assertEquals("Hello Hyyan", resolver.resolve("greeting", Locale.ENGLISH, "Hyyan"));
      assertEquals("Bonjour Marie", resolver.resolve("greeting", Locale.FRENCH, "Marie"));
      assertEquals("Hallo Hans", resolver.resolve("greeting", Locale.GERMAN, "Hans"));
    }

    @Test
    void shouldResolveKeyWithMultipleParameters() {
      assertEquals("You have 5 new messages", resolver.resolve("message.count", Locale.ENGLISH, 5));
    }

    @Test
    void shouldFallbackToDefaultBundleForUnknownLocale() {
      assertEquals("Welcome", resolver.resolve("welcome", Locale.JAPANESE));
    }

    @Test
    void shouldUseDefaultLocaleWhenNullLocalePassed() {
      assertEquals("Welcome", resolver.resolve("welcome", null));
    }
  }

  @Nested
  class BatchResolution {

    @Test
    void shouldResolveMultipleKeys() {
      List<String> keys = Arrays.asList("welcome", "farewell", "nonexistent");
      Map<String, String> results = resolver.resolve(keys, Locale.ENGLISH);

      assertEquals(3, results.size());
      assertEquals("Welcome", results.get("welcome"));
      assertEquals("Goodbye", results.get("farewell"));
      assertEquals("nonexistent", results.get("nonexistent"));
    }

    @Test
    void shouldResolveMultipleKeysForDifferentLocale() {
      List<String> keys = Arrays.asList("welcome", "farewell");
      Map<String, String> results = resolver.resolve(keys, Locale.FRENCH);

      assertEquals(2, results.size());
      assertEquals("Bienvenue", results.get("welcome"));
      assertEquals("Au revoir", results.get("farewell"));
    }
  }
}
