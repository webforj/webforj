package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasLabelTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetLabel() {
    String expectedLabel = "Test Label";
    assertSame(component.setLabel(expectedLabel), component);
    assertEquals(expectedLabel, component.getLabel());
  }

  class CompositeMock extends Composite<ConcernComponentMock> implements HasLabel<CompositeMock> {
  }
}
