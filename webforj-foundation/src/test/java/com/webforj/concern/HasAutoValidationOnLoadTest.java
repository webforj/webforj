package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasAutoValidationOnLoadTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetAutoValidateOnLoad() {
    boolean expectedAutoValidate = false;

    assertSame(component, component.setAutoClientValidateOnLoad(expectedAutoValidate));
    assertEquals(expectedAutoValidate, component.isAutoClientValidateOnLoad());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasClientAutoValidationOnLoad<CompositeMock> {
  }
}
