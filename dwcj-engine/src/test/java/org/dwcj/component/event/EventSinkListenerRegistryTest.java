package org.dwcj.component.event;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.stream.Stream;
import org.dwcj.component.DwcComponentMock;
import org.dwcj.component.event.sink.EventSinkMock;
import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.dispatcher.ListenerRegistration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class EventSinkListenerRegistryTest {
  EventSinkListenerRegistry<MouseEnterEvent> registry;
  EventDispatcher dispatcher;
  EventSinkMock sink;

  @BeforeEach
  void setUp() {
    dispatcher = new EventDispatcher();
    sink = spy(new EventSinkMock(new DwcComponentMock(), dispatcher, 0));
    registry = new EventSinkListenerRegistry<>(sink, MouseEnterEvent.class);
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

    registry.addEventListener(e -> {});
    registry.addEventListener(e -> {});

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
  void shouldReturnDifferentCallbackIdWhenMultipleCallbacks(){
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
}
