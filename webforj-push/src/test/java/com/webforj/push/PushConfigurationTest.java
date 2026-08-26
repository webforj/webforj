package com.webforj.push;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;
import com.webforj.push.exception.WebforjPushException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class PushConfigurationTest {

  private static Config config(Map<String, Object> values) {
    return ConfigFactory.parseMap(values);
  }

  @Nested
  class FromConfig {

    @Test
    void shouldBeEmptyWhenNothingIsSet() {
      assertTrue(PushConfiguration.fromConfig(config(Map.of())).isEmpty());
      assertTrue(PushConfiguration.fromConfig(null).isEmpty());
    }

    @Test
    void shouldReadAllThreeKeys() {
      Optional<PushConfiguration> configuration = PushConfiguration.fromConfig(
          config(Map.of(PushConfiguration.PUBLIC_KEY, "pub ", PushConfiguration.PRIVATE_KEY, "priv",
              PushConfiguration.SUBJECT, "mailto:ops@example.com")));

      assertTrue(configuration.isPresent());
      assertEquals("pub", configuration.get().getKeys().getPublicKey());
      assertEquals("priv", configuration.get().getKeys().getPrivateKey());
      assertEquals("mailto:ops@example.com", configuration.get().getSubject());
    }

    @Test
    void shouldNameEveryMissingKeyWhenPartiallyConfigured() {
      WebforjPushException e = assertThrows(WebforjPushException.class,
          () -> PushConfiguration.fromConfig(config(Map.of(PushConfiguration.PUBLIC_KEY, "pub"))));

      assertEquals(PushStatus.NOT_CONFIGURED, e.getStatus());
      assertTrue(e.getMessage().contains(PushConfiguration.PRIVATE_KEY), e.getMessage());
      assertTrue(e.getMessage().contains(PushConfiguration.SUBJECT), e.getMessage());
      assertFalse(e.getMessage().contains(PushConfiguration.PUBLIC_KEY + ","), e.getMessage());
      assertTrue(e.getMessage().contains("webforj:push-keys"), e.getMessage());
    }

    @Test
    void shouldTreatBlankValuesAsMissing() {
      Map<String, Object> values = new HashMap<>();
      values.put(PushConfiguration.PUBLIC_KEY, " ");
      values.put(PushConfiguration.PRIVATE_KEY, "");
      values.put(PushConfiguration.SUBJECT, "");

      assertTrue(PushConfiguration.fromConfig(config(values)).isEmpty());
    }

    @Test
    void shouldRejectTheSubjectThatIsNotMailtoOrHttps() {
      WebforjPushException e = assertThrows(WebforjPushException.class,
          () -> PushConfiguration.fromConfig(
              config(Map.of(PushConfiguration.PUBLIC_KEY, "pub", PushConfiguration.PRIVATE_KEY,
                  "priv", PushConfiguration.SUBJECT, "ops@example.com"))));

      assertTrue(e.getMessage().contains(PushConfiguration.SUBJECT), e.getMessage());
    }

    @Test
    void shouldAcceptAnHttpsSubject() {
      PushConfiguration configuration =
          new PushConfiguration(new PushKeys("pub", "priv"), "https://example.com");

      assertEquals("https://example.com", configuration.getSubject());
    }

    @Test
    void shouldRejectMissingKeys() {
      assertThrows(WebforjPushException.class,
          () -> new PushConfiguration(null, "mailto:ops@example.com"));
    }
  }

  @Nested
  class Require {

    @Test
    void shouldFailNamingTheKeysWhenNotConfigured() {
      WebforjPushException e = assertThrows(WebforjPushException.class,
          () -> PushConfiguration.require(config(Map.of())));

      assertTrue(e.getMessage().contains(PushConfiguration.PUBLIC_KEY), e.getMessage());
      assertTrue(e.getMessage().contains(PushConfiguration.PRIVATE_KEY), e.getMessage());
      assertTrue(e.getMessage().contains(PushConfiguration.SUBJECT), e.getMessage());
    }

    @Test
    void shouldReturnTheConfigurationWhenSet() {
      PushConfiguration configuration = PushConfiguration
          .require(config(Map.of(PushConfiguration.PUBLIC_KEY, "pub", PushConfiguration.PRIVATE_KEY,
              "priv", PushConfiguration.SUBJECT, "mailto:ops@example.com")));

      assertEquals("pub", configuration.getKeys().getPublicKey());
    }
  }
}
