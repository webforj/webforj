package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasMaxTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetMaxInteger() {
    Double expectedMax = 100d;
    assertSame((component.setMax(expectedMax)), component);
    assertEquals(expectedMax, component.getMax());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasMax<CompositeMock, Double> {
  }
}
