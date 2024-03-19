package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasMinTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetMin() {
    Double expectedMin = 1.99;
    assertSame(component.setMin(expectedMin), component);
    assertEquals(expectedMin, component.getMin());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasMin<CompositeMock, Double> {
  }
}
