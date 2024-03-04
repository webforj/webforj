package com.webforj.component.drawer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.drawer.event.DrawerCloseEvent;
import com.webforj.component.drawer.event.DrawerOpenEvent;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.dispatcher.EventListener;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class DrawerTest {
  Drawer component;

  @BeforeEach
  void setUp() {
    component = new Drawer();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Drawer.class, component, descriptor -> {
          return !Arrays.asList("opened").contains(descriptor.getName());
        });
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldShowHideDrawer() {
      component.open();

      assertEquals(true, component.isOpened());
      assertEquals(true, component.getOriginalElement().getProperty("opened"));

      component.close();

      assertEquals(false, component.isOpened());
      assertEquals(false, component.getOriginalElement().getProperty("opened"));
    }
  }

  @Nested
  @DisplayName("Slots API")
  class SlotsApi {

    @Test
    void shouldAddToTitle() {
      Component title = mock(Component.class);
      component.addToTitle(title);
      assertEquals(title, component.getOriginalElement().getFirstComponentInSlot("title"));
    }

    @Test
    void shouldAddToHeaderActions() {
      Component headerActions = mock(Component.class);
      component.addToHeaderActions(headerActions);
      assertEquals(headerActions,
          component.getOriginalElement().getFirstComponentInSlot("header-actions"));
    }

    @Test
    void shouldAddToFooter() {
      Component footer = mock(Component.class);
      component.addToFooter(footer);
      assertEquals(footer, component.getOriginalElement().getFirstComponentInSlot("footer"));
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddOpenListener() {
      component.onOpen(event -> {
      });

      List<EventListener<DrawerOpenEvent>> listeners =
          component.getEventListeners(DrawerOpenEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<DrawerOpenEvent>);
    }

    @Test
    void shouldAddCloseListener() {
      component.onClose(event -> {
      });

      List<EventListener<DrawerCloseEvent>> listeners =
          component.getEventListeners(DrawerCloseEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<DrawerCloseEvent>);
    }
  }
}
