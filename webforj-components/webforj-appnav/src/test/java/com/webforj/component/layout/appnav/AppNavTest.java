package com.webforj.component.layout.appnav;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.layout.appnav.event.AppNavLocationChangedEvent;
import com.webforj.dispatcher.EventListener;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AppNavTest {

  AppNav component;

  @BeforeEach
  void setUp() {
    component = new AppNav();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(AppNav.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddNavigateListener() {
      component.onLocationChanged(event -> {
      });

      List<EventListener<AppNavLocationChangedEvent>> listeners =
          component.getEventListeners(AppNavLocationChangedEvent.class);

      assertEquals(2, listeners.size());
      assertTrue(listeners.get(1) instanceof EventListener<AppNavLocationChangedEvent>);
    }
  }
}
