package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasStyleTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetStyle() {
    assertSame(component.setStyle("color", "red"), component);
    assertEquals("red", component.getStyle("color"));
  }

  @Test
  void shouldGetComputedStyle() {
    component.setStyle("color", "red");
    assertEquals("red", component.getComputedStyle("color"));
  }

  @Test
  void shouldRemoveStyle() {
    component.setStyle("color", "red");
    assertEquals("red", component.getStyle("color"));

    assertSame(component.removeStyle("color"), component);
    assertNull(component.getStyle("color"));
  }

  class CompositeMock extends Composite<ConcernComponentMock> implements HasStyle<CompositeMock> {
  }
}
