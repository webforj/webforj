package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.webforj.bbj.security.fixture.AlphaOwnerEvaluator;
import org.junit.jupiter.api.Test;

class BBjRouteSecurityEvaluatorTest {

  @Test
  void shouldUseThePriority10ByDefault() {
    assertEquals(10, new AlphaOwnerEvaluator().getPriority());
  }
}
