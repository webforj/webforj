package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasMinLengthTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetMinLength() {
    int expectedMaxLength = 10;
    assertSame(component, component.setMinLength(expectedMaxLength));
    assertEquals(expectedMaxLength, component.getMinLength());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasMinLength<CompositeMock> {
  }
}
