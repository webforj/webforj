package com.webforj.dispatcher;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.EventObject;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiPredicate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class EventDispatcherTest {

  private EventDispatcher dispatcher;

  @BeforeEach
  void setUp() {
    dispatcher = new EventDispatcher();
  }

  @Test
  void testAddListener() {
    EventListener<ClickEvent> listener = event -> {
    };
    ListenerRegistration<ClickEvent> registration =
        dispatcher.addListener(ClickEvent.class, listener);
    assertTrue(dispatcher.hasListener(ClickEvent.class, listener));
    assertEquals(1, dispatcher.getCount(ClickEvent.class));

    // Check ListenerRegistration methods
    assertNotNull(registration);
    assertEquals(ClickEvent.class, registration.getEventClass());
  }

  @Test
  void testRemoveListener() {
    EventListener<ClickEvent> listener = event -> {
    };
    ListenerRegistration<ClickEvent> registration =
        dispatcher.addListener(ClickEvent.class, listener);
    assertTrue(dispatcher.hasListener(ClickEvent.class, listener));

    // Remove the listener using ListenerRegistration
    registration.remove();
    assertFalse(dispatcher.hasListener(ClickEvent.class, listener));
  }

  @Test
  void testRemoveAllListenersOfType() {
    EventListener<ClickEvent> clickListener1 = event -> {
    };
    EventListener<ClickEvent> clickListener2 = event -> {
    };
    EventListener<MouseEvent> mouseEvent = event -> {
    };

    dispatcher.addListener(ClickEvent.class, clickListener1);
    dispatcher.addListener(ClickEvent.class, clickListener2);
    dispatcher.addListener(MouseEvent.class, mouseEvent);

    dispatcher.removeAllListeners(ClickEvent.class);

    assertFalse(dispatcher.hasListener(ClickEvent.class));
    assertTrue(dispatcher.hasListener(MouseEvent.class, mouseEvent));
  }

  @Test
  void testRemoveAllListeners() {
    EventListener<ClickEvent> clickListener1 = event -> {
    };
    EventListener<ClickEvent> clickListener2 = event -> {
    };
    EventListener<MouseEvent> mouseEvent = event -> {
    };

    dispatcher.addListener(ClickEvent.class, clickListener1);
    dispatcher.addListener(ClickEvent.class, clickListener2);
    dispatcher.addListener(MouseEvent.class, mouseEvent);

    dispatcher.removeAllListeners();

    assertFalse(dispatcher.hasListener(ClickEvent.class));
    assertFalse(dispatcher.hasListener(MouseEvent.class, mouseEvent));
  }

  @Test
  void testGetEventListeners() {
    EventListener<ClickEvent> clickListener1 = event -> {
    };
    EventListener<ClickEvent> clickListener2 = event -> {
    };
    EventListener<MouseEvent> mouseEvent = event -> {
    };

    dispatcher.addListener(ClickEvent.class, clickListener1);
    dispatcher.addListener(ClickEvent.class, clickListener2);
    dispatcher.addListener(MouseEvent.class, mouseEvent);

    assertEquals(2, dispatcher.getListeners(ClickEvent.class).size());
    assertEquals(1, dispatcher.getListeners(MouseEvent.class).size());
  }

  @Test
  void testDispatchEvent() {
    // Create a custom event and listener for testing
    ComponentMock component = new ComponentMock();
    Map<String, Object> eventMap = new HashMap<>() {
      {
        put("x", 50);
        put("y", 120);
      }
    };
    MouseEvent customMouseEvent = new MouseEvent(component, eventMap);

    // Create a listener that checks if the event data matches
    EventListener<MouseEvent> listener = event -> {
      assertSame(eventMap, event.getData());
    };

    dispatcher.addListener(MouseEvent.class, listener);
    dispatcher.dispatchEvent(customMouseEvent);
  }

  @Test
  void testDispatchEventWithFilter() {
    // Create mock components and events for testing
    ComponentMock component = new ComponentMock();
    Map<String, Object> eventMap = new HashMap<>();
    ClickEvent clickEvent = new ClickEvent(component, eventMap);

    // Create mock listeners
    EventListener<ClickEvent> clickListener1 = mock(EventListener.class);
    EventListener<ClickEvent> clickListener2 = mock(EventListener.class);
    EventListener<MouseEvent> mouseListener = mock(EventListener.class);

    // Add listeners to the dispatcher
    dispatcher.addListener(ClickEvent.class, clickListener1);
    dispatcher.addListener(ClickEvent.class, clickListener2);
    dispatcher.addListener(MouseEvent.class, mouseListener);

    // Define a filter that only allows the second click listener to be notified
    BiPredicate<EventListener<ClickEvent>, ClickEvent> filter =
        (listener, event) -> listener.equals(clickListener2);

    // Dispatch the click event with the filter
    dispatcher.dispatchEvent(clickEvent, filter);

    verify(clickListener1, times(0)).onEvent(any(ClickEvent.class));
    verify(mouseListener, times(0)).onEvent(any(MouseEvent.class));

    // Verify that the second click listener was called exactly once
    verify(clickListener2, times(1)).onEvent(clickEvent);
  }

  private class ComponentMock {
  }

  private class ClickEvent extends EventObject {
    public ClickEvent(Object source, Map<String, Object> data) {
      super(source);
    }
  }

  private class MouseEvent extends EventObject {
    private final Map<String, Object> data;

    public MouseEvent(Object source, Map<String, Object> data) {
      super(source);
      this.data = data;
    }

    public Map<String, Object> getData() {
      return data;
    }
  }
}
