package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasAutoValidationTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetAutoValidate() {
    boolean expectedAutoValidate = false;

    assertSame(component, component.setAutoClientValidate(expectedAutoValidate));
    assertEquals(expectedAutoValidate, component.isAutoClientValidate());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasClientAutoValidation<CompositeMock> {
  }
}
