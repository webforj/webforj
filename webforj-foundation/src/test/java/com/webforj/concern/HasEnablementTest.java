package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasEnablementTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldEnableDisableComponent() {
    assertSame(component.setEnabled(false), component);
    assertFalse(component.isEnabled());

    component.setEnabled(true);
    assertTrue(component.isEnabled());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasEnablement<CompositeMock> {
  }
}
