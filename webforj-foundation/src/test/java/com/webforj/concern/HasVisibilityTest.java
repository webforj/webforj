package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasVisibilityTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetVisibility() {
    assertSame(component.setVisible(false), component);
    assertFalse(component.isVisible());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasVisibility<CompositeMock> {
  }
}
