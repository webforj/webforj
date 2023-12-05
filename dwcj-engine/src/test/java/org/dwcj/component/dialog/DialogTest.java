package org.dwcj.component.dialog;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;
import java.util.List;
import org.dwcj.component.Component;
import org.dwcj.component.dialog.event.DialogCloseEvent;
import org.dwcj.component.dialog.event.DialogOpenEvent;
import org.dwcj.component.element.PropertyDescriptorTester;
import org.dwcj.dispatcher.EventListener;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

class DialogTest {

  Dialog component;

  @BeforeEach
  void setUp() {
    component = new Dialog();
  }

  @Nested
  @DisplayName("Properties API")
  class PropertiesApi {

    @Test
    void shouldSetGetProperties() {
      try {
        PropertyDescriptorTester.run(Dialog.class, component, descriptor -> {
          return !Arrays.asList("opened").contains(descriptor.getName());
        });
      } catch (Exception e) {
        fail("PropertyDescriptor test failed: " + e.getMessage());
      }
    }

    @Test
    void shouldShowHideDialog() {
      component.open();

      assertEquals(true, component.isOpened());
      assertEquals(true, component.getOriginalElement().getProperty("opened"));

      component.close();

      assertEquals(false, component.isOpened());
      assertEquals(false, component.getOriginalElement().getProperty("opened"));
    }

    @ParameterizedTest
    @ValueSource(booleans = {true, false})
    void shouldDisableClosingTheDialog(boolean closeable) {
      Dialog spy = spy(component);
      spy.setCloseable(closeable);

      verify(spy, times(1)).setCancelOnOutsideClick(closeable);
      verify(spy, times(1)).setCancelOnEscKey(closeable);
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
      assertEquals(content, component.getOriginalElement().getFirstComponentInSlot("content"));
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

      List<EventListener<DialogOpenEvent>> listeners =
          component.getEventListeners(DialogOpenEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<DialogOpenEvent>);
    }

    @Test
    void shouldAddCloseListener() {
      component.onClose(event -> {
      });

      List<EventListener<DialogCloseEvent>> listeners =
          component.getEventListeners(DialogCloseEvent.class);

      assertEquals(1, listeners.size());
      assertTrue(listeners.get(0) instanceof EventListener<DialogCloseEvent>);
    }
  }
}
