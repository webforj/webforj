package com.webforj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.webforj.component.DwcComponentMock;
import com.webforj.component.event.sink.EventSinkMock;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.dispatcher.EventListener;
import com.webforj.dispatcher.ListenerRegistration;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;

class EventSinkListenerRegistryTest {
  ComponentEventSinkRegistry<MouseEnterEvent> registry;
  EventDispatcher dispatcher;
  EventSinkMock sink;

  @BeforeEach
  void setUp() {
    dispatcher = new EventDispatcher();
    sink = spy(new EventSinkMock(new DwcComponentMock(), dispatcher, 0));
    registry = new ComponentEventSinkRegistry<>(sink, MouseEnterEvent.class);
  }

  static Stream<Arguments> connectionStates() {
    return Stream.of(Arguments.of(true, true), Arguments.of(true, false), Arguments.of(false, true),
        Arguments.of(false, false));
  }

  @ParameterizedTest
  @MethodSource("connectionStates")
  @DisplayName("Test adding event listeners with different connection and callback states")
  void canAddEventListener(boolean isConnected, boolean isMultipleCallbacks) {
    when(sink.isConnected()).thenReturn(isConnected);
    when(sink.isMultipleCallbacks()).thenReturn(isMultipleCallbacks);

    registry.addEventListener(e -> {
    });
    registry.addEventListener(e -> {
    });

    int expectedVerify = !isConnected ? 0 : isMultipleCallbacks ? 2 : 1;

    assertEquals(2, dispatcher.getCount(MouseEnterEvent.class));
    verify(sink, times(expectedVerify)).setCallback(null);
  }

  @ParameterizedTest
  @MethodSource("connectionStates")
  @DisplayName("Test removing event listeners with different connection and callback states")
  void canRemoveEventListener(boolean isConnected, boolean isMultipleCallbacks) {
    when(sink.isConnected()).thenReturn(isConnected);
    when(sink.isMultipleCallbacks()).thenReturn(isMultipleCallbacks);

    ListenerRegistration<MouseEnterEvent> r1 = registry.addEventListener(e -> {
    });
    ListenerRegistration<MouseEnterEvent> r2 = registry.addEventListener(e -> {
    });

    assertEquals(2, dispatcher.getCount(MouseEnterEvent.class));

    r1.remove();
    assertEquals(1, dispatcher.getCount(MouseEnterEvent.class));
    verify(sink, times(!isConnected ? 0 : isMultipleCallbacks ? 1 : 0)).removeCallback(anyString());

    r2.remove();
    assertEquals(0, dispatcher.getCount(MouseEnterEvent.class));
    verify(sink, times(!isConnected ? 0 : isMultipleCallbacks ? 2 : 1)).removeCallback(anyString());
  }

  @Test
  @DisplayName("Should return the same callback ID for single callback")
  void shouldReturnSameCallbackIdWhenSingleCallback() {
    when(sink.isConnected()).thenReturn(true);
    when(sink.isMultipleCallbacks()).thenReturn(false);

    ListenerRegistration<MouseEnterEvent> r1 = registry.addEventListener(e -> {
    });
    ListenerRegistration<MouseEnterEvent> r2 = registry.addEventListener(e -> {
    });

    assertEquals(2, dispatcher.getCount(MouseEnterEvent.class));
    verify(sink, times(1)).setCallback(any());

    String callbackId = registry.getCallbackId(r1.getListener());
    assertEquals(callbackId, registry.getCallbackId(r2.getListener()));
  }

  @Test
  @DisplayName("Should return different callback IDs for multiple callbacks")
  void shouldReturnDifferentCallbackIdWhenMultipleCallbacks() {
    when(sink.isConnected()).thenReturn(true);
    when(sink.isMultipleCallbacks()).thenReturn(true);

    ListenerRegistration<MouseEnterEvent> r1 = registry.addEventListener(e -> {
    });
    ListenerRegistration<MouseEnterEvent> r2 = registry.addEventListener(e -> {
    });

    assertEquals(2, dispatcher.getCount(MouseEnterEvent.class));
    verify(sink, times(2)).setCallback(any());

    String callbackId1 = registry.getCallbackId(r1.getListener());
    String callbackId2 = registry.getCallbackId(r2.getListener());
    assertNotEquals(callbackId1, callbackId2);
  }

  @Test
  void shouldNotKeepCollectionsForEventsWithoutListeners() throws ReflectiveOperationException {
    when(sink.isConnected()).thenReturn(false);

    registry.attach();

    assertNull(field(registry, "registrations"));
    assertNull(field(registry, "callbackIds"));
    assertNull(registry.getCallbackId(e -> {
    }));
  }

  @Test
  void shouldConnectListenersAddedBeforeAttachAndForgetThemAfterwards()
      throws ReflectiveOperationException {
    when(sink.isConnected()).thenReturn(false);
    when(sink.isMultipleCallbacks()).thenReturn(true);
    EventListener<MouseEnterEvent> listener = e -> {
    };
    registry.addEventListener(listener);
    verify(sink, times(0)).setCallback(null);

    when(sink.isConnected()).thenReturn(true);
    doReturn("cb-1").when(sink).setCallback(null);
    registry.attach();
    registry.attach();

    verify(sink, times(1)).setCallback(null);
    assertEquals("cb-1", registry.getCallbackId(listener));
    assertNull(field(registry, "registrations"));
  }

  @Test
  void shouldNotConnectAListenerRemovedBeforeAttach() {
    when(sink.isConnected()).thenReturn(false);
    registry.addEventListener(e -> {
    }).remove();

    when(sink.isConnected()).thenReturn(true);
    registry.attach();

    verify(sink, times(0)).setCallback(any());
  }

  @ParameterizedTest
  @ValueSource(booleans = {true, false})
  void shouldForgetTheCallbackIdOfARemovedListener(boolean isMultipleCallbacks) {
    when(sink.isConnected()).thenReturn(true);
    when(sink.isMultipleCallbacks()).thenReturn(isMultipleCallbacks);
    doReturn("cb-1").when(sink).setCallback(null);
    EventListener<MouseEnterEvent> listener = e -> {
    };

    registry.addEventListener(listener).remove();

    verify(sink, times(1)).removeCallback("cb-1");
    assertNull(registry.getCallbackId(listener));
  }

  @Test
  void shouldObserveTheComponentOnceWhenTheListenerIsAddedAndRemovedRepeatedly() {
    DwcComponentMock component = spy(new DwcComponentMock());
    EventSinkMock repeatedSink = spy(new EventSinkMock(component, dispatcher, 0));
    when(repeatedSink.isConnected()).thenReturn(true);
    when(repeatedSink.isMultipleCallbacks()).thenReturn(false);
    doReturn("cb-1").when(repeatedSink).setCallback(null);
    registry = new ComponentEventSinkRegistry<>(repeatedSink, MouseEnterEvent.class);

    for (int i = 0; i < 3; i++) {
      registry.addEventListener(e -> {
      }).remove();
    }

    verify(component, times(1)).addLifecycleObserver(any());
  }

  @Test
  void shouldForgetEveryCallbackIdWhenTheComponentIsDestroyed() {
    DwcComponentMock component = new DwcComponentMock();
    EventSinkMock destroyedSink = spy(new EventSinkMock(component, dispatcher, 0));
    when(destroyedSink.isConnected()).thenReturn(true);
    when(destroyedSink.isMultipleCallbacks()).thenReturn(false);
    doReturn("cb-1").when(destroyedSink).setCallback(null);
    registry = new ComponentEventSinkRegistry<>(destroyedSink, MouseEnterEvent.class);
    EventListener<MouseEnterEvent> first = e -> {
    };
    EventListener<MouseEnterEvent> second = e -> {
    };
    registry.addEventListener(first).remove();
    registry.addEventListener(second);

    component.destroy();

    assertNull(registry.getCallbackId(first));
    assertNull(registry.getCallbackId(second));
  }

  private static Object field(Object target, String name) throws ReflectiveOperationException {
    java.lang.reflect.Field f = EventSinkListenerRegistry.class.getDeclaredField(name);
    f.setAccessible(true);
    return f.get(target);
  }
}
