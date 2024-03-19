package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasTooltipTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetTextTooltip() {
    assertSame(component, component.setTooltipText("Hello, World!"));
    assertEquals("Hello, World!", component.getTooltipText());
  }

  class CompositeMock extends Composite<ConcernComponentMock> implements HasTooltip<CompositeMock> {
  }
}
