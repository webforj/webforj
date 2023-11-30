package org.dwcj.component.drawer;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.Arrays;
import java.util.List;
import org.dwcj.component.drawer.event.DrawerCloseEvent;
import org.dwcj.component.drawer.event.DrawerOpenEvent;
import org.dwcj.component.element.PropertyDescriptorTester;
import org.dwcj.dispatcher.EventListener;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

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
          return !Arrays.asList("placement", "opened").contains(descriptor.getName());
        });
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @ParameterizedTest
    @EnumSource(Drawer.Placement.class)
    void shouldSetGetPlacement(Drawer.Placement placement) {
      component.setPlacement(placement);

      assertEquals(placement, component.getPlacement());
      assertEquals(placement.getValue(), component.getOriginalElement().getProperty("placement"));
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
