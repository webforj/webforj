package org.dwcj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.dwcj.component.event.sink.MouseEnterEventSink;
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
    manager.addEventListener(e -> {
    });
    manager.addEventListener(e -> {
    });
    assertEquals(2, dispatcher.getCount(MouseEnterEvent.class));
    verify(sink, times(1)).setCallback();
  }

  @Nested
  @DisplayName("removeCallback")
  class RemoveCallback {
    @Test
    void canRemoveEventListener() {
      ComponentEventListener<MouseEnterEvent> listener1 = e -> {
      };
      ComponentEventListener<MouseEnterEvent> listener2 = e -> {
      };

      ListenerRegistration<MouseEnterEvent> r1 = manager.addEventListener(listener1);
      ListenerRegistration<MouseEnterEvent> r2 = manager.addEventListener(listener2);
      assertEquals(2, dispatcher.getCount(MouseEnterEvent.class));

      r1.remove();
      assertEquals(1, dispatcher.getCount(MouseEnterEvent.class));
      verify(sink, times(0)).removeCallback();

      r2.remove();
      assertEquals(0, dispatcher.getCount(MouseEnterEvent.class));
      verify(sink, times(1)).removeCallback();
    }

    @Test
    void canRemoveEventListenerFromRegistration() {

      ListenerRegistration<MouseEnterEvent> registration1 = manager.addEventListener(e -> {
      });
      ListenerRegistration<MouseEnterEvent> registration2 = manager.addEventListener(e -> {
      });

      assertEquals(2, dispatcher.getCount(MouseEnterEvent.class));

      registration1.remove();
      assertEquals(1, dispatcher.getCount(MouseEnterEvent.class));
      verify(sink, times(0)).removeCallback();

      registration2.remove();
      assertEquals(0, dispatcher.getCount(MouseEnterEvent.class));
      verify(sink, times(1)).removeCallback();
    }

    @Test
    void clearControlCallbackWhenNoListeners() throws IllegalAccessException {
      ListenerRegistration<MouseEnterEvent> r1 = manager.addEventListener(e -> {
      });
      ListenerRegistration<MouseEnterEvent> r2 = manager.addEventListener(e -> {
      });

      r1.remove();

      assertEquals(1, dispatcher.getCount(MouseEnterEvent.class));
      verify(sink, times(0)).removeCallback();

      r2.remove();

      assertEquals(0, dispatcher.getCount(MouseEnterEvent.class));
      verify(sink, times(1)).removeCallback();
    }
  }

  @Nested
  @DisplayName("CatchUp")
  class CatchUp {

    @Test
    void catchUpWithOneListenerRegistered() {
      ComponentEventListener<MouseEnterEvent> listener = e -> {
      };

      manager.addEventListener(listener);
      manager.attach();

      verify(sink, times(2)).setCallback();
    }

    @Test
    void catchUpWithNoListenersRegistered() {
      manager.attach();

      verify(sink, times(0)).setCallback();
    }
  }
}
