package org.dwcj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;
import org.dwcj.component.ComponentMock;
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
    ComponentEventListener<ClickEvent> listener = event -> {
    };
    ListenerRegistration<ClickEvent> registration =
        dispatcher.addListener(ClickEvent.class, listener);
    assertTrue(dispatcher.hasListener(ClickEvent.class, listener));
    assertEquals(1, dispatcher.getCount(ClickEvent.class));

    // Check ListenerRegistration methods
    assertNotNull(registration);
    assertEquals(ClickEvent.class, registration.getEventClass());
    assertFalse(registration.hasMore());
  }

  @Test
  void testRemoveListener() {
    ComponentEventListener<ClickEvent> listener = event -> {
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
    ComponentEventListener<ClickEvent> clickListener1 = event -> {
    };
    ComponentEventListener<ClickEvent> clickListener2 = event -> {
    };
    ComponentEventListener<MouseEvent> mouseEvent = event -> {
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
    ComponentEventListener<ClickEvent> clickListener1 = event -> {
    };
    ComponentEventListener<ClickEvent> clickListener2 = event -> {
    };
    ComponentEventListener<MouseEvent> mouseEvent = event -> {
    };

    dispatcher.addListener(ClickEvent.class, clickListener1);
    dispatcher.addListener(ClickEvent.class, clickListener2);
    dispatcher.addListener(MouseEvent.class, mouseEvent);

    dispatcher.removeAllListeners();

    assertFalse(dispatcher.hasListener(ClickEvent.class));
    assertFalse(dispatcher.hasListener(MouseEvent.class, mouseEvent));
  }

  @Test
  void testListenersCount() {
    ComponentEventListener<ClickEvent> listener1 = event -> {
    };
    ComponentEventListener<ClickEvent> listener2 = event -> {
    };
    ListenerRegistration<ClickEvent> registration1 =
        dispatcher.addListener(ClickEvent.class, listener1);
    ListenerRegistration<ClickEvent> registration2 =
        dispatcher.addListener(ClickEvent.class, listener2);

    assertEquals(2, dispatcher.getCount(ClickEvent.class));

    // Check ListenerRegistration methods
    assertEquals(2, registration1.getCount());
    assertEquals(2, registration2.getCount());

    // Remove one of the listeners
    registration1.remove();
    assertEquals(1, dispatcher.getCount(ClickEvent.class));
    assertEquals(1, registration2.getCount());

    assertTrue(registration1.hasMore());
    assertFalse(registration2.hasMore());

    // Remove the other listener
    registration2.remove();

    assertFalse(registration1.hasMore());
    assertFalse(registration2.hasMore());
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
    ComponentEventListener<MouseEvent> listener = event -> {
      assertSame(eventMap, event.getData());
      assertSame(component, event.getComponent());
    };

    dispatcher.addListener(MouseEvent.class, listener);
    dispatcher.dispatchEvent(customMouseEvent);
  }

  @Test
  void testGetEventListeners() {
    ComponentEventListener<ClickEvent> clickListener1 = event -> {
    };
    ComponentEventListener<ClickEvent> clickListener2 = event -> {
    };
    ComponentEventListener<MouseEvent> mouseEvent = event -> {
    };

    dispatcher.addListener(ClickEvent.class, clickListener1);
    dispatcher.addListener(ClickEvent.class, clickListener2);
    dispatcher.addListener(MouseEvent.class, mouseEvent);

    assertEquals(2, dispatcher.getListeners(ClickEvent.class).size());
    assertEquals(1, dispatcher.getListeners(MouseEvent.class).size());
  }

  private class ClickEvent extends ComponentEvent<ComponentMock> {
    public ClickEvent(ComponentMock control, Map<String, Object> data) {
      super(control, data);
    }
  }

  private class MouseEvent extends ComponentEvent<ComponentMock> {
    public MouseEvent(ComponentMock control, Map<String, Object> data) {
      super(control, data);
    }
  }
}
