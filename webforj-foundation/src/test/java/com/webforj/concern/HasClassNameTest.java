package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasClassNameTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldAddClassName() {
    assertSame(component.addClassName("testClass"), component);
    assertEquals("testClass", component.getClassName());
  }

  @Test
  void shouldRemoveClassName() {
    component.addClassName("testClass");
    assertEquals("testClass", component.getClassName());

    component.removeClassName("testClass");
    assertNull(component.getClassName());
  }

  class CompositeMock extends Composite<ConcernComponentMock>
      implements HasClassName<CompositeMock> {

    public String getClassName() {
      return getBoundComponent().getClassName();
    }
  }
}
