package com.webforj.concern;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.webforj.component.Component;
import com.webforj.component.Composite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class HasComponentsTest {

  private CompositeMock component;

  @BeforeEach
  void setup() {
    component = new CompositeMock();
  }

  @Test
  void shouldAddRemoveComponent() {
    Component child = mock(Component.class);
    component.add(child);
    assertTrue(component.hasComponent(child));
    assertEquals(1, component.getComponentCount());

    component.remove(child);
    assertFalse(component.hasComponent(child));
    assertEquals(0, component.getComponentCount());
  }

  @Test
  void shouldRemoveAllComponents() {
    component.add(new ConcernComponentMock(), new ConcernComponentMock());
    assertEquals(2, component.getComponentCount());

    component.removeAll();
    assertEquals(0, component.getComponentCount());
  }

  @Test
  void shouldGetComponent() {
    Component child = mock(Component.class);
    when(child.getComponentId()).thenReturn("test");
    component.add(child);

    assertEquals(child, component.getComponent("test"));
  }

  class CompositeMock extends Composite<ConcernComponentMock> implements HasComponents {
  }
}
