package com.webforj.spring.testbed;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.spring.ContextHolder;
import com.webforj.spring.testbed.WebforjTestbedContextCustomizerFactory.WebforjTestbedCustomizer;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = WebforjTestbedContextCustomizerFactoryTest.EmptyConfiguration.class)
class WebforjTestbedContextCustomizerFactoryTest {

  static class EmptyConfiguration {
  }

  @Autowired
  private ApplicationContext context;

  @Test
  void shouldExposePlainTestContextsWithoutBootAutoConfiguration() {
    assertNotNull(context);
    assertSame(context, ContextHolder.getContext());
  }

  @Test
  void shouldContributeOneCustomizerPerContext() {
    WebforjTestbedContextCustomizerFactory factory = new WebforjTestbedContextCustomizerFactory();

    Object first = factory.createContextCustomizer(getClass(), List.of());
    Object second = factory.createContextCustomizer(getClass(), List.of());

    assertNotNull(first);
    assertNotSame(first, second);
    assertEquals(first, second);
    assertEquals(first.hashCode(), second.hashCode());
  }

  @Test
  void shouldCompareCustomizersByType() {
    WebforjTestbedCustomizer customizer = new WebforjTestbedCustomizer();

    assertEquals(customizer, new WebforjTestbedCustomizer());
    assertNotEquals(customizer, new Object());
  }
}
