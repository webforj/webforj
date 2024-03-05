package com.webforj.component.googlecharts;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.component.googlecharts.events.GoogleChartReadyEvent;
import com.webforj.component.googlecharts.events.GoogleChartSelectedEvent;
import com.webforj.dispatcher.EventListener;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class GoogleChartTest {

  GoogleChart component;

  @BeforeEach
  void setUp() {
    component = new GoogleChart(GoogleChart.Type.PIE);
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(GoogleChart.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }
  }

  @Nested
  @DisplayName("Events API")
  class EventsApi {

    @Test
    void shouldAddOpenListener() {
      component.onSelect(event -> {
      });

      List<EventListener<GoogleChartSelectedEvent>> listeners =
          component.getEventListeners(GoogleChartSelectedEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<GoogleChartSelectedEvent>);
    }

    @Test
    void shouldAddCloseListener() {
      EventListener<GoogleChartReadyEvent> listener = event -> {
      };
      component.onReady(listener);

      List<EventListener<GoogleChartReadyEvent>> listeners =
          component.getEventListeners(GoogleChartReadyEvent.class);

      assertTrue(listeners.contains(listener));
    }
  }

  @Test
  void shouldReturnEmptyImageUriWhenNoDataAvailable() {
    String blank = "data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw==";
    assertEquals(blank, component.getImageUri());
  }
}
