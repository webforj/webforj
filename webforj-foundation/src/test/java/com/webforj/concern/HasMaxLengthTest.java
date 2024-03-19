package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasMaxLengthTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetMaxLength() {
    int expectedMaxLength = 256;
    assertSame(component.setMaxLength(expectedMaxLength), component);
    assertEquals(expectedMaxLength, component.getMaxLength());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasMaxLength<CompositeMock> {
  }
}
