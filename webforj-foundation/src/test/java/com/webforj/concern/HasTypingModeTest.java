package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasTypingModeTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetTypingMode() {
    assertSame(component.setTypingMode(HasTypingMode.TypingMode.OVERWRITE), component);
    assertEquals(HasTypingMode.TypingMode.OVERWRITE, component.getTypingMode());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasTypingMode<CompositeMock> {
  }
}
