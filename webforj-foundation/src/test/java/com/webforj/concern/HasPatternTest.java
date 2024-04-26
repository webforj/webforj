package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasPatternTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetPattern() {
    String expectedPattern = "[0-9]{3}";
    assertSame(component, component.setPattern(expectedPattern));
    assertEquals(expectedPattern, component.getPattern());
  }

  class CompositeMock extends Composite<ConcernComponentMock> implements HasPattern<CompositeMock> {
  }
}
