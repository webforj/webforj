package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasReadOnlyTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldEnableDisableReadonlyMode() {
    assertSame(component, component.setReadOnly(false));
    assertFalse(component.isReadOnly());

    component.setReadOnly(true);
    assertTrue(component.isReadOnly());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasReadOnly<CompositeMock> {
  }
}
