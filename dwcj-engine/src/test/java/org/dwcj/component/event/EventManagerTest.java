package org.dwcj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.dwcj.component.event.sink.MouseEnterEventSink;
import org.dwcj.component.label.Label;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/** EventHandler tests. */
@ExtendWith(MockitoExtension.class)
public class EventManagerTest {

  @Mock
  Label control;

  @Mock
  EventController<MouseEnterEvent> eventController;

  @InjectMocks
  EventManager<Label> eventManager;

  @DisplayName("Register Event")
  @Test
  void addEvent() {
    eventManager.addEvent(MouseEnterEventSink.class, MouseEnterEvent.class);

    assertEquals(1, eventManager.getEventMap().size());
    assertNotNull(eventManager.getEventMap().get(MouseEnterEvent.class));
  }

  @DisplayName("Throwing when adding invalid event")
  @Test
  void throwingWhenAddingInvalidEvent() {
    Assertions.assertThrows(DwcjRuntimeException.class, () -> {
      eventManager.addEvent(null, MouseEnterEvent.class);
    });

    assertEquals(0, eventManager.getEventMap().size());
  }

  @DisplayName("Add EventListener")
  @Test
  void addEventListener() {
    eventManager.addEvent(MouseEnterEventSink.class, MouseEnterEvent.class);

    EventListener<MouseEnterEvent> listener = e -> {
    };

    eventManager.addEventListener(MouseEnterEvent.class, listener);

    assertEquals(1, eventManager.getEventDispatcher().getListenersCount(MouseEnterEvent.class));
  }

  @DisplayName("Add EventListener for unregistered Event")
  @Test
  void addEventListenerForUnRegisteredEvent() {
    EventListener<MouseEnterEvent> listener = e -> {
    };

    eventManager.addEventListener(MouseEnterEvent.class, listener);

    assertEquals(0, eventManager.getEventDispatcher().getListenersCount(MouseEnterEvent.class));

  }

  @DisplayName("Remove EventListener")
  @Test
  void removeEventListener() {
    eventManager.addEvent(MouseEnterEventSink.class, MouseEnterEvent.class);

    EventListener<MouseEnterEvent> listener = e -> {
    };

    eventManager.addEventListener(MouseEnterEvent.class, listener);
    eventManager.removeEventListener(MouseEnterEvent.class, listener);

    assertEquals(0, eventManager.getEventDispatcher().getListenersCount(MouseEnterEvent.class));
  }

  @DisplayName("Remove EventListener for unregistered Event")
  @Test
  void removeEventListenerForUnRegisteredEvent() {
    eventManager.addEvent(MouseEnterEventSink.class, MouseEnterEvent.class);
    EventListener<MouseEnterEvent> listener = e -> {
    };

    EventListener<MouseExitEvent> listener2 = e -> {
    };

    eventManager.addEventListener(MouseEnterEvent.class, listener);
    eventManager.removeEventListener(MouseExitEvent.class, listener2);

    assertEquals(1, eventManager.getEventDispatcher().getListenersCount(MouseEnterEvent.class));
  }

  @DisplayName("catching all registered events up")
  @Test
  void catchUp() {
    eventManager.getEventMap().put(MouseEnterEvent.class, eventController);
    eventManager.catchUp();

    verify(eventController, times(1)).catchUp();
  }

  @DisplayName("CatchUp when no events are registered.")
  @Test
  void catchUpWhenNoEventsAreRegistered() {
    eventManager.catchUp();

    verify(eventController, times(0)).catchUp();
  }



}
