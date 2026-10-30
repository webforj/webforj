package com.webforj.bbj.security;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

class BBjRouteSecurityEvaluatorTest {

  @Test
  void shouldUsePriorityTenByDefault() {
    BBjRouteSecurityEvaluator evaluator = new DefaultPriorityEvaluator();

    assertEquals(10, evaluator.getPriority());
  }
}
