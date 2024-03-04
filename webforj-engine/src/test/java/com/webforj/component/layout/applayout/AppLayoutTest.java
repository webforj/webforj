package com.webforj.component.layout.applayout;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.layout.applayout.event.AppLayoutDrawerCloseEvent;
import com.webforj.component.layout.applayout.event.AppLayoutDrawerOpenEvent;
import com.webforj.dispatcher.EventListener;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AppLayoutTest {

  AppLayout component;

  @BeforeEach
  void setUp() {
    component = new AppLayout();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(AppLayout.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }

  @Nested
  @DisplayName("Slots API")
  class SlotsApi {

    @Test
    void shouldAddToHeader() {
      Component header = mock(Component.class);
      component.addToHeader(header);
      assertEquals(header, component.getOriginalElement().getFirstComponentInSlot("header"));
    }

    @Test
    void shouldAddToContent() {
      Component content = mock(Component.class);
      component.addToContent(content);
      assertTrue(component.getOriginalElement().hasComponent(content));
    }

    @Test
    void shouldAddToFooter() {
      Component footer = mock(Component.class);
      component.addToFooter(footer);
      assertEquals(footer, component.getOriginalElement().getFirstComponentInSlot("footer"));
    }

    @Test
    void shouldAddToDrawer() {
      Component drawer = mock(Component.class);
      component.addToDrawer(drawer);
      assertEquals(drawer, component.getOriginalElement().getFirstComponentInSlot("drawer"));
    }

    @Test
    void shouldAddToDrawerTitle() {
      Component drawerTitle = mock(Component.class);
      component.addToDrawerTitle(drawerTitle);
      assertEquals(drawerTitle,
          component.getOriginalElement().getFirstComponentInSlot("drawer-title"));
    }

    @Test
    void shouldAddToHeaderActions() {
      Component headerActions = mock(Component.class);
      component.addToDrawerHeaderActions(headerActions);
      assertEquals(headerActions,
          component.getOriginalElement().getFirstComponentInSlot("drawer-header-actions"));
    }

    @Test
    void shouldAddToDrawerFooter() {
      Component drawerFooter = mock(Component.class);
      component.addToDrawerFooter(drawerFooter);
      assertEquals(drawerFooter,
          component.getOriginalElement().getFirstComponentInSlot("drawer-footer"));
    }

  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddOpenListener() {
      component.onDrawerOpen(event -> {
      });

      List<EventListener<AppLayoutDrawerOpenEvent>> listeners =
          component.getEventListeners(AppLayoutDrawerOpenEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<AppLayoutDrawerOpenEvent>);
    }

    @Test
    void shouldAddCloseListener() {
      component.onDrawerClose(event -> {
      });

      List<EventListener<AppLayoutDrawerCloseEvent>> listeners =
          component.getEventListeners(AppLayoutDrawerCloseEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<AppLayoutDrawerCloseEvent>);
    }
  }
}
