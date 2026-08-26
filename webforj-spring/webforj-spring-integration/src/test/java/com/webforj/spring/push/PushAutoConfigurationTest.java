package com.webforj.spring.push;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.interaso.webpush.VapidKeys;
import com.webforj.push.PushConfiguration;
import com.webforj.push.PushKeys;
import com.webforj.push.PushSender;
import com.webforj.push.PushStatus;
import com.webforj.push.exception.WebforjPushException;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

class PushAutoConfigurationTest {

  private final VapidKeys keys = VapidKeys.generate();
  private final ApplicationContextRunner runner = new ApplicationContextRunner()
      .withConfiguration(AutoConfigurations.of(PushAutoConfiguration.class));

  @Test
  void shouldExposeTheSenderWhenTheKeysAreSet() {
    runner.withPropertyValues(properties()).run(context -> {
      PushSender sender = context.getBean(PushSender.class);

      assertEquals("mailto:ops@example.com", sender.getSubject());
      assertEquals(keys.getX509PublicKey(), sender.getKeys().getPublicKey());
    });
  }

  @Test
  void shouldStayOutWhenNoKeyIsSet() {
    runner.run(context -> assertFalse(context.containsBean("webforjPushSender")));
  }

  @Test
  void shouldFailTheContextNamingTheMissingKeysWhenOnlyThePublicKeyIsSet() {
    runner.withPropertyValues("webforj.push.public-key=abc").run(context -> {
      Throwable failure = rootCause(context.getStartupFailure());

      WebforjPushException e = assertInstanceOf(WebforjPushException.class, failure);
      assertEquals(PushStatus.NOT_CONFIGURED, e.getStatus());
      assertTrue(e.getMessage().contains(PushConfiguration.PRIVATE_KEY), e.getMessage());
      assertTrue(e.getMessage().contains(PushConfiguration.SUBJECT), e.getMessage());
    });
  }

  @Test
  void shouldFailTheContextWhenThePublicKeyIsMissing() {
    runner.withPropertyValues("webforj.push.private-key=" + keys.getPkcs8PrivateKey(),
        "webforj.push.subject=mailto:ops@example.com").run(context -> {
          Throwable failure = rootCause(context.getStartupFailure());

          WebforjPushException e = assertInstanceOf(WebforjPushException.class, failure);
          assertEquals(PushStatus.NOT_CONFIGURED, e.getStatus());
          assertTrue(e.getMessage().contains(PushConfiguration.PUBLIC_KEY), e.getMessage());
        });
  }

  @Test
  void shouldYieldToAnApplicationSender() {
    PushSender custom = new PushSender(
        new PushKeys(keys.getX509PublicKey(), keys.getPkcs8PrivateKey()), "https://example.com");

    runner.withPropertyValues(properties()).withBean("custom", PushSender.class, () -> custom)
        .run(context -> {
          assertFalse(context.containsBean("webforjPushSender"));
          assertSame(custom, context.getBean(PushSender.class));
        });
  }

  private String[] properties() {
    return new String[] {"webforj.push.public-key=" + keys.getX509PublicKey(),
        "webforj.push.private-key=" + keys.getPkcs8PrivateKey(),
        "webforj.push.subject=mailto:ops@example.com"};
  }

  private static Throwable rootCause(Throwable failure) {
    assertNotNull(failure, "a partial configuration fails at startup");
    Throwable cause = failure;
    while (cause.getCause() != null && !(cause instanceof WebforjPushException)) {
      cause = cause.getCause();
    }

    return cause;
  }
}
