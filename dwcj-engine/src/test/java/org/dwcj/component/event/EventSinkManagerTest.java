package org.dwcj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/** EventHandler tests. */
@ExtendWith(MockitoExtension.class)
class EventSinkManagerTest {
  @Mock
  MouseEnterEventSink sink;

  @Mock
  MouseEnterEvent event;

  @InjectMocks
  EventSinkManager<MouseEnterEvent> controller;

  @BeforeEach
  void setUp() throws IllegalAccessException {
    FieldUtils.writeField(controller, "event", MouseEnterEvent.class, true);
    FieldUtils.writeField(controller, "dispatcher", new EventDispatcher(), true);
  }

  @DisplayName("adding an eventListener")
  @Test
  void addEventListener() throws IllegalAccessException {
    EventListener<MouseEnterEvent> listener = e -> {
    };

    controller.addEventListener(listener);
    verify(sink, times(1)).setCallback();

    EventDispatcher dispatcher =
        (EventDispatcher) FieldUtils.readField(controller, "dispatcher", true);
    assertEquals(1, dispatcher.getListenersCount(MouseEnterEvent.class));
  }

  @DisplayName("removing eventListener after being added.")
  @Test
  void removeEventListener() {
    EventListener<MouseEnterEvent> listener = e -> {
    };

    controller.addEventListener(listener);
    controller.removeEventListener(listener);

    verify(sink, times(1)).removeCallback();
  }

  @DisplayName("removing one listener when they are more then one registered.")
  @Test
  void removeEventListenerWhenMoreThenOneListenersAreRegistered() throws IllegalAccessException {
    EventListener<MouseEnterEvent> listener = e -> {
    };

    EventListener<MouseEnterEvent> listener2 = a -> {
    };

    controller.addEventListener(listener);
    controller.addEventListener(listener2);


    controller.removeEventListener(listener);
    EventDispatcher dispatcher =
        (EventDispatcher) FieldUtils.readField(controller, "dispatcher", true);

    assertEquals(1, dispatcher.getListenersCount(MouseEnterEvent.class));
    verify(sink, times(0)).removeCallback();
  }

  @DisplayName("catchUp with one listener registered.")
  @Test
  void catchUp() {
    EventListener<MouseEnterEvent> listener = e -> {
    };

    controller.addEventListener(listener);
    controller.catchUp();

    verify(sink, times(2)).setCallback();
  }


  @DisplayName("catchUp with no Listeners registered.")
  @Test
  void catchUpWhenNoListeners() {
    controller.catchUp();

    verify(sink, times(0)).setCallback();
  }
}
