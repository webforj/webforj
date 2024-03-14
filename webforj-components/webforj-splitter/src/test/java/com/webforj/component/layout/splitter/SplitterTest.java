package com.webforj.component.layout.splitter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.layout.splitter.event.SplitterResizeEvent;
import com.webforj.dispatcher.EventListener;
import java.util.Arrays;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class SplitterTest {

  Splitter component;

  @BeforeEach
  void setUp() {
    component = new Splitter();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Splitter.class, component, descriptor -> {
          return !Arrays.asList("positionRelative").contains(descriptor.getName());
        });
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldValidatePositionRelative() {
      assertThrows(IllegalArgumentException.class, () -> component.setPositionRelative(150));
      assertThrows(IllegalArgumentException.class, () -> component.setPositionRelative(-50));
    }
  }

  @Nested
  @DisplayName("Slots API")
  class SlotsApi {

    @Test
    void shouldAddToMaster() {
      Component master = mock(Component.class);
      component.addToMaster(master);
      assertEquals(master, component.getOriginalElement().getFirstComponentInSlot("master"));
    }

    @Test
    void shouldAddToDetail() {
      Component detail = mock(Component.class);
      component.addToDetail(detail);
      assertEquals(detail, component.getOriginalElement().getFirstComponentInSlot("detail"));
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddResizeListener() {
      component.onResize(event -> {
      });

      List<EventListener<SplitterResizeEvent>> listeners =
          component.getEventListeners(SplitterResizeEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<SplitterResizeEvent>);
    }
  }
}
