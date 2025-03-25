package com.webforj.component.infinitescroll;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.icons.FeatherIcon;
import com.webforj.component.infinitescroll.event.InfiniteScrollEvent;
import com.webforj.dispatcher.EventListener;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class InfiniteScrollTest {

  InfiniteScroll component;

  @BeforeEach
  void setUp() {
    component = new InfiniteScroll();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(InfiniteScroll.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldSetIconFromIconComponent() {
      component.setIcon(FeatherIcon.LOADER.create());
      assertEquals("feather:loader", component.getIcon());
    }
  }

  @Nested
  @DisplayName("Slots API")
  class SlotsApi {

    @Test
    void shouldAddToContent() {
      Component header = mock(Component.class);
      component.addToContent(header);
      assertTrue(component.getOriginalElement().hasComponent(header));
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddScrollListener() {
      component.onScroll(event -> {
      });

      List<EventListener<InfiniteScrollEvent>> listeners =
          component.getEventListeners(InfiniteScrollEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<InfiniteScrollEvent>);
    }
  }
}
