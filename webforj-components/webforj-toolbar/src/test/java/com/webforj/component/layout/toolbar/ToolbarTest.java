package com.webforj.component.layout.toolbar;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptorTester;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class ToolbarTest {

  Toolbar component;

  @BeforeEach
  void setUp() {
    component = new Toolbar();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Toolbar.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }

  @Nested
  @DisplayName("Slots API")
  class SlotsApi {

    @Test
    void shouldAddToStart() {
      Component header = mock(Component.class);
      component.addToStart(header);
      assertEquals(header,
          component.getOriginalElement().getFirstComponentInSlot(Toolbar.START_SLOT));
    }

    @Test
    void shouldAddToTitle() {
      Component header = mock(Component.class);
      component.addToTitle(header);
      assertEquals(header,
          component.getOriginalElement().getFirstComponentInSlot(Toolbar.TITLE_SLOT));
    }

    @Test
    void shouldAddToContent() {
      Component header = mock(Component.class);
      component.addToContent(header);
      assertTrue(component.getOriginalElement().hasComponent(header));
    }

    @Test
    void shouldAddToEnd() {
      Component header = mock(Component.class);
      component.addToEnd(header);
      assertEquals(header,
          component.getOriginalElement().getFirstComponentInSlot(Toolbar.END_SLOT));
    }
  }
}
