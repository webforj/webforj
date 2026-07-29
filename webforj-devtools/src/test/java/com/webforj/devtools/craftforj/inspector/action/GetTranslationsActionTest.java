package com.webforj.devtools.craftforj.inspector.action;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandler;
import com.webforj.devtools.craftforj.inspector.contribution.FeatureHandlerRegistry;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class GetTranslationsActionTest {

  private static final String CORE_TEST_BUNDLE =
      "com.webforj.devtools.craftforj.inspector.action.testDescriptions";
  private static final String CONTRIB_TEST_BUNDLE =
      "com.webforj.devtools.craftforj.inspector.action.testContrib";
  private static final String MISSING_BUNDLE =
      "com.webforj.devtools.craftforj.inspector.action.nope";

  @Test
  @DisplayName("Should return correct action name with inspector prefix")
  void shouldReturnCorrectActionName() {
    GetTranslationsAction action = new GetTranslationsAction();

    assertEquals("inspector.getTranslations", action.getAction());
  }

  @Nested
  @DisplayName("Locale resolution")
  class LocaleResolution {

    @Test
    @DisplayName("Should default to en when params is empty")
    void shouldDefaultToEnglishWhenParamsEmpty() {
      GetTranslationsAction action =
          new GetTranslationsAction(new FeatureHandlerRegistry(), CORE_TEST_BUNDLE);

      GetTranslationsAction.Response response = action.handle(new JsonObject());

      assertEquals("en", response.getLocale());
    }

    @Test
    @DisplayName("Should default to en when locale is blank")
    void shouldDefaultToEnglishWhenLocaleBlank() {
      GetTranslationsAction action =
          new GetTranslationsAction(new FeatureHandlerRegistry(), CORE_TEST_BUNDLE);
      JsonObject params = new JsonObject();
      params.addProperty("locale", "  ");

      GetTranslationsAction.Response response = action.handle(params);

      assertEquals("en", response.getLocale());
    }

    @Test
    @DisplayName("Should default to en when locale is null")
    void shouldDefaultToEnglishWhenLocaleNull() {
      GetTranslationsAction action =
          new GetTranslationsAction(new FeatureHandlerRegistry(), CORE_TEST_BUNDLE);
      JsonObject params = new JsonObject();
      params.add("locale", JsonNull.INSTANCE);

      GetTranslationsAction.Response response = action.handle(params);

      assertEquals("en", response.getLocale());
    }

    @Test
    @DisplayName("Should use the requested locale tag")
    void shouldUseRequestedLocale() {
      GetTranslationsAction action =
          new GetTranslationsAction(new FeatureHandlerRegistry(), CORE_TEST_BUNDLE);
      JsonObject params = new JsonObject();
      params.addProperty("locale", "de");

      GetTranslationsAction.Response response = action.handle(params);

      assertEquals("de", response.getLocale());
    }
  }

  @Nested
  @DisplayName("Core bundle loading")
  class CoreBundleLoading {

    @Test
    @DisplayName("Should load the base catalog for the default locale")
    void shouldLoadBaseCatalog() {
      GetTranslationsAction action =
          new GetTranslationsAction(new FeatureHandlerRegistry(), CORE_TEST_BUNDLE);

      GetTranslationsAction.Response response = action.handle(new JsonObject());

      Map<String, String> translations = response.getTranslations();
      assertEquals("Sets the text content.", translations.get("props.HasText.desc"));
      assertEquals("Core value.", translations.get("props.Shared.desc"));
    }

    @Test
    @DisplayName("Should fall back to the base catalog for missing locale-specific keys")
    void shouldFallBackToBaseCatalog() {
      GetTranslationsAction action =
          new GetTranslationsAction(new FeatureHandlerRegistry(), CORE_TEST_BUNDLE);
      JsonObject params = new JsonObject();
      params.addProperty("locale", "de");

      GetTranslationsAction.Response response = action.handle(params);

      Map<String, String> translations = response.getTranslations();
      assertEquals("Setzt den Textinhalt.", translations.get("props.HasText.desc"));
      assertEquals("Core value.", translations.get("props.Shared.desc"));
    }

    @Test
    @DisplayName("Should not fall back to the JVM default locale when the base locale is requested")
    void shouldIgnoreJvmDefaultLocale() {
      Locale previousDefault = Locale.getDefault();
      Locale.setDefault(Locale.GERMANY);
      try {
        GetTranslationsAction action =
            new GetTranslationsAction(new FeatureHandlerRegistry(), CORE_TEST_BUNDLE);
        JsonObject params = new JsonObject();
        params.addProperty("locale", "en");

        GetTranslationsAction.Response response = action.handle(params);

        assertEquals("Sets the text content.",
            response.getTranslations().get("props.HasText.desc"));
      } finally {
        Locale.setDefault(previousDefault);
      }
    }

    @Test
    @DisplayName("Should return an empty map when the core bundle cannot be found")
    void shouldReturnEmptyMapWhenCoreBundleMissing() {
      GetTranslationsAction action =
          new GetTranslationsAction(new FeatureHandlerRegistry(), MISSING_BUNDLE);

      GetTranslationsAction.Response response = action.handle(new JsonObject());

      assertTrue(response.getTranslations().isEmpty());
    }
  }

  @Nested
  @DisplayName("Contributed bundle merging")
  class ContributedBundleMerging {

    @Test
    @DisplayName("Should merge a contributed bundle and let it override the core bundle")
    void shouldMergeContributedBundle() {
      FeatureHandlerRegistry registry = mock(FeatureHandlerRegistry.class);
      FeatureHandler handler = mock(FeatureHandler.class);
      when(handler.getTranslationBundle()).thenReturn(CONTRIB_TEST_BUNDLE);
      when(registry.getHandlers()).thenReturn(List.of(handler));

      GetTranslationsAction action = new GetTranslationsAction(registry, CORE_TEST_BUNDLE);

      GetTranslationsAction.Response response = action.handle(new JsonObject());

      Map<String, String> translations = response.getTranslations();
      assertEquals("Sets the text content.", translations.get("props.HasText.desc"));
      assertEquals("Contrib override.", translations.get("props.Shared.desc"));
      assertEquals("Contributed only.", translations.get("props.Contrib.desc"));
    }

    @Test
    @DisplayName("Should skip handlers that contribute no bundle")
    void shouldSkipNullBundleContribution() {
      FeatureHandlerRegistry registry = mock(FeatureHandlerRegistry.class);
      FeatureHandler handler = mock(FeatureHandler.class);
      when(handler.getTranslationBundle()).thenReturn(null);
      when(registry.getHandlers()).thenReturn(List.of(handler));

      GetTranslationsAction action = new GetTranslationsAction(registry, CORE_TEST_BUNDLE);

      GetTranslationsAction.Response response = action.handle(new JsonObject());

      assertEquals("Core value.", response.getTranslations().get("props.Shared.desc"));
    }

    @Test
    @DisplayName("Should only load a distinct bundle once even if contributed by multiple handlers")
    void shouldDeduplicateContributedBundles() {
      FeatureHandlerRegistry registry = mock(FeatureHandlerRegistry.class);
      FeatureHandler first = mock(FeatureHandler.class);
      FeatureHandler second = mock(FeatureHandler.class);
      when(first.getTranslationBundle()).thenReturn(CONTRIB_TEST_BUNDLE);
      when(second.getTranslationBundle()).thenReturn(CONTRIB_TEST_BUNDLE);
      when(registry.getHandlers()).thenReturn(List.of(first, second));

      GetTranslationsAction action = new GetTranslationsAction(registry, CORE_TEST_BUNDLE);

      GetTranslationsAction.Response response = action.handle(new JsonObject());

      assertEquals("Contributed only.", response.getTranslations().get("props.Contrib.desc"));
    }

    @Test
    @DisplayName("Should skip a contributed bundle that cannot be resolved without failing")
    void shouldSkipUnresolvableContributedBundle() {
      FeatureHandlerRegistry registry = mock(FeatureHandlerRegistry.class);
      FeatureHandler handler = mock(FeatureHandler.class);
      when(handler.getTranslationBundle()).thenReturn(MISSING_BUNDLE);
      when(registry.getHandlers()).thenReturn(List.of(handler));

      GetTranslationsAction action = new GetTranslationsAction(registry, CORE_TEST_BUNDLE);

      GetTranslationsAction.Response response = action.handle(new JsonObject());

      assertFalse(response.getTranslations().isEmpty());
      assertEquals("Core value.", response.getTranslations().get("props.Shared.desc"));
    }
  }
}
