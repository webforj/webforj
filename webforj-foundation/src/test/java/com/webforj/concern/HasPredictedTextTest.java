package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasPredictedTextTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetText() {
    assertSame(component.setPredictedText("Hello, World!"), component);
    assertEquals("Hello, World!", component.getPredictedText());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasPredictedText<CompositeMock> {
  }
}
