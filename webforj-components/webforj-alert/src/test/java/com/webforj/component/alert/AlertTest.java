package com.webforj.component.alert;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;

import com.webforj.component.Component;
import com.webforj.component.Theme;
import com.webforj.component.alert.event.AlertCloseEvent;
import com.webforj.component.element.PropertyDescriptorTester;
import com.webforj.dispatcher.EventListener;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AlertTest {

  Alert component;

  @BeforeEach
  void setUp() {
    component = new Alert();
  }


  @Nested
  @DisplayName("Constructors")
  class Constructors {

    @Test
    void shouldCreateAlertWithDefaultConstructor() {
      Alert alert = new Alert();
      assertEquals("", alert.getText());
      assertEquals(Theme.DEFAULT, alert.getTheme());
      assertEquals(false, alert.isClosable());
    }

    @Test
    void shouldCreateAlertWithTextConstructor() {
      Alert alert = new Alert("Test Message");
      assertEquals("Test Message", alert.getText());
      assertEquals(Theme.DEFAULT, alert.getTheme());
      assertEquals(false, alert.isClosable());
    }

    @Test
    void shouldCreateAlertWithTextAndThemeConstructor() {
      Alert alert = new Alert("Test Message", Theme.GRAY);
      assertEquals("Test Message", alert.getText());
      assertEquals(Theme.GRAY, alert.getTheme());
      assertEquals(false, alert.isClosable());
    }

    @Test
    void shouldCreateAlertWithTextThemeAndClosableConstructor() {
      Alert alert = new Alert("Test Message", Theme.GRAY, true);
      assertEquals("Test Message", alert.getText());
      assertEquals(Theme.GRAY, alert.getTheme());
      assertEquals(true, alert.isClosable());
    }
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Alert.class, component);
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldShowHideAlert() {
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
    void shouldAddCloseListener() {
      component.onClose(event -> {
      });

      List<EventListener<AlertCloseEvent>> listeners =
          component.getEventListeners(AlertCloseEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<AlertCloseEvent>);
    }
  }
}
