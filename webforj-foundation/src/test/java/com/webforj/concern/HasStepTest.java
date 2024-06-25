package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasStepTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetValue() {
    Double expectedValue = 1.99;
    assertSame(component, component.setStep(expectedValue));
    assertEquals(expectedValue, component.getStep());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasStep<CompositeMock, Double> {
  }
}
