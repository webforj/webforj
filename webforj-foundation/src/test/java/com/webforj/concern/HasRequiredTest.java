package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasRequiredTest {
  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldEnableDisableRequired() {
    assertSame(component, component.setRequired(false));
    assertFalse(component.isRequired());

    component.setRequired(true);
    assertTrue(component.isRequired());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasRequired<CompositeMock> {
  }
}
