package org.dwcj.component.event;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

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
class EventControllerTest {
  @Mock
  MouseEnterEventSink sink;

  @Mock
  MouseEnterEvent event;

  @InjectMocks
  EventController<MouseEnterEvent> controller;

  @BeforeEach
  void setUp() {
    controller.setDispatcher(new EventDispatcher());
  }

  @DisplayName("adding an eventListener")
  @Test
  void addEventListener() {
    EventListener<MouseEnterEvent> listener = e -> {
    };

    controller.addEventListener(listener);
    verify(sink, times(1)).setCallback();
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
  void removeEventListenerWhenMoreThenOneListenersAreRegistered() {
    EventListener<MouseEnterEvent> listener = e -> {
    };

    EventListener<MouseEnterEvent> listener2 = e -> {
    };

    controller.addEventListener(listener);
    controller.addEventListener(listener2);

    controller.removeEventListener(listener);

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