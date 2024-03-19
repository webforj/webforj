package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasPropertyTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetProperty() {
    assertSame(component.setProperty("name", "value"), component);
    assertEquals("value", component.getProperty("name"));
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasProperty<CompositeMock> {
  }
}
