package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasPlaceholderTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetPlaceholder() {
    String expectedPlaceholder = "Test Placeholder";
    assertSame(component, component.setPlaceholder(expectedPlaceholder));
    assertEquals(expectedPlaceholder, component.getPlaceholder());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasPlaceholder<CompositeMock> {
  }
}
