package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasAttributeTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldSetGetAttribute() {
    assertSame(component.setAttribute("name", "value"), component);
    assertEquals("value", component.getAttribute("name"));
  }

  @Test
  void shouldReportWhetherAttributeExists() {
    assertFalse(component.hasAttribute("name"));

    component.setAttribute("name", "value");
    assertTrue(component.hasAttribute("name"));

    component.removeAttribute("name");
    assertFalse(component.hasAttribute("name"));
  }

  @Test
  void shouldRemoveAttribute() {
    component.setAttribute("name", "value");
    assertEquals("value", component.getAttribute("name"));

    component.removeAttribute("name");
    assertNull(component.getAttribute("name"));
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasAttribute<CompositeMock> {
  }
}
