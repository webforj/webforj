package org.dwcj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.dwcj.component.event.EventDispatcher;
import org.dwcj.component.event.EventListener;
import org.dwcj.component.event.MouseEnterEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class EventSinkListenerRegistryTest {
  EventSinkListenerRegistry<MouseEnterEvent> manager;
  EventDispatcher dispatcher;

  @Mock
  MouseEnterEventSink sink;

  @BeforeEach
  void setUp() {
    dispatcher = new EventDispatcher();
    doReturn(dispatcher).when(sink).getEventDispatcher();
    manager = new EventSinkListenerRegistry<>(sink, MouseEnterEvent.class);
  }

  @Test
  void canAddEventListener() {
    EventListener<MouseEnterEvent> listener = e -> {
    };

    assertNotNull(sink.getEventDispatcher());
    manager.addEventListener(listener);
    assertEquals(1, dispatcher.getListenersCount(MouseEnterEvent.class));
  }

  @Nested
  @DisplayName("removeCallback")
  class RemoveCallback {
    @Test
    void canRemoveEventListener() {
      EventListener<MouseEnterEvent> listener = e -> {
      };

      manager.addEventListener(listener);
      assertEquals(1, dispatcher.getListenersCount(MouseEnterEvent.class));

      manager.removeEventListener(listener);
      assertEquals(0, dispatcher.getListenersCount(MouseEnterEvent.class));
    }

    @Test
    void clearControlCallbackWhenNoListeners() throws IllegalAccessException {
      EventListener<MouseEnterEvent> listener = e -> {
      };

      EventListener<MouseEnterEvent> listener2 = a -> {
      };

      manager.addEventListener(listener);
      manager.addEventListener(listener2);

      manager.removeEventListener(listener);

      assertEquals(1, dispatcher.getListenersCount(MouseEnterEvent.class));
      verify(sink, times(0)).removeCallback();

      manager.removeEventListener(listener2);

      assertEquals(0, dispatcher.getListenersCount(MouseEnterEvent.class));
      verify(sink, times(1)).removeCallback();
    }
  }

  @Nested
  @DisplayName("removeCallback")
  class CatchUp {

    @Test
    void catchUpWithOneListenerRegistered() {
      EventListener<MouseEnterEvent> listener = e -> {
      };

      manager.addEventListener(listener);
      manager.catchUp();

      verify(sink, times(2)).setCallback();
    }

    @Test
    void catchUpWithNoListenersRegistered() {
      manager.catchUp();

      verify(sink, times(0)).setCallback();
    }
  }
}
