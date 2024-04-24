package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import com.webforj.concern.HasClientValidationStyle.ValidationStyle;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasValidationStyleTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetValidationStyle() {
    ValidationStyle expectedValidationStyle = ValidationStyle.INLINE;
    assertSame(component, component.setValidationStyle(expectedValidationStyle));
    assertEquals(expectedValidationStyle, component.getValidationStyle());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasClientValidationStyle<CompositeMock> {
  }
}
