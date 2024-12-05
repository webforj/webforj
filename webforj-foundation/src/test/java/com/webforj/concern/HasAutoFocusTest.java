package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasAutoFocusTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetAutofocus() {
    boolean expectedAutofocus = true;

    assertSame(component, component.setAutoFocus(expectedAutofocus));
    assertEquals(expectedAutofocus, component.isAutoFocus());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasAutoFocus<CompositeMock> {
  }
}
