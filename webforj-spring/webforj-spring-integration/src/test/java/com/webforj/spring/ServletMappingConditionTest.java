package com.webforj.spring;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.annotation.ConditionContext;
import org.springframework.core.type.AnnotatedTypeMetadata;
import org.springframework.mock.env.MockEnvironment;

@ExtendWith(MockitoExtension.class)
class ServletMappingConditionTest {

  @Mock
  private ConditionContext conditionContext;

  @Mock
  private AnnotatedTypeMetadata metadata;

  private MockEnvironment environment;
  private ServletMappingCondition condition;

  @BeforeEach
  void setUp() {
    condition = new ServletMappingCondition();
    environment = new MockEnvironment();
  }

  @Test
  void shouldMatchWhenMappingIsRootDefault() {
    when(conditionContext.getEnvironment()).thenReturn(environment);
    environment.setProperty("webforj.servletMapping", "/*");

    assertTrue(condition.matches(conditionContext, metadata));
  }

  @Test
  void shouldNotMatchWhenMappingIsNotRoot() {
    when(conditionContext.getEnvironment()).thenReturn(environment);
    environment.setProperty("webforj.servletMapping", "/app/*");

    assertFalse(condition.matches(conditionContext, metadata));
  }
}
