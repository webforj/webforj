package com.webforj.environment.namespace;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import com.basis.bbj.proxies.BBjAPI;
import com.basis.bbj.proxies.BBjNamespace;
import com.basis.bbj.proxies.event.BBjNamespaceEvent;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import com.webforj.Environment;
import com.webforj.bridge.WebforjBBjBridge;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.environment.namespace.event.NamespaceEvent;
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class NamespaceEventSinkTest {

  NamespaceEventSink eventSink;
  BBjNamespace bbjNamespace;
  EventDispatcher dispatcher;

  @BeforeEach
  void setUp() throws BBjException {
    BBjAPI api = mock(BBjAPI.class);
    WebforjBBjBridge bridge = mock(WebforjBBjBridge.class);
    Environment.init(api, bridge, 0);

    bbjNamespace = mock(BBjNamespace.class);
    dispatcher = mock(EventDispatcher.class);
    Namespace namespace = new GlobalNamespace();
    namespace.setBbjNamespace(bbjNamespace);

    eventSink = new NamespaceEventSink(namespace, dispatcher) {
      @Override
      protected void doSetCallback(BBjNamespace namespace, CustomObject handler, String callback)
          throws BBjException {
        namespace.setCallbackForNamespace(handler, callback);
      }

      @Override
      protected void doRemoveCallback(BBjNamespace namespace) throws BBjException {
        namespace.removeCallbackForNamespace();
      }

      @Override
      protected NamespaceEvent createEvent(String key, Object oldValue, Object newValue) {
        return new NamespaceEvent(this.getComponent(), key, oldValue, newValue) {};
      }
    };
  }

  @AfterEach
  void tearDown() {
    Environment.cleanup();
  }

  @Test
  void shouldSetCallback() throws BBjException {
    eventSink.setCallback(null);
    verify(bbjNamespace, times(1)).setCallbackForNamespace(any(), any(String.class));
  }

  @Test
  void shouldThrowExceptionWhenSetCallbackFails() throws BBjException {
    doThrow(BBjException.class).when(bbjNamespace).setCallbackForNamespace(any(),
        any(String.class));
    assertThrows(WebforjRuntimeException.class, () -> eventSink.setCallback(null));
  }

  @Test
  void shouldRemoveCallback() throws BBjException {
    eventSink.removeCallback(null);
    verify(bbjNamespace, times(1)).removeCallbackForNamespace();
  }

  @Test
  void shouldThrowExceptionWhenRemoveCallbackFails() throws BBjException {
    doThrow(BBjException.class).when(bbjNamespace).removeCallbackForNamespace();
    assertThrows(WebforjRuntimeException.class, () -> eventSink.removeCallback(null));
  }

  @Test
  void shouldHandleEvent() {
    BBjNamespaceEvent bbjEvent = mock(BBjNamespaceEvent.class);
    when(bbjEvent.getNewValue()).thenReturn("newValue");
    when(bbjEvent.getOldValue()).thenReturn("oldValue");
    when(bbjEvent.getVariableName()).thenReturn("key");

    eventSink.handleEvent(bbjEvent);

    verify(dispatcher, times(1)).dispatchEvent(any(NamespaceEvent.class));
  }

  @Test
  void shouldNotRemoveCallbackWhenListenersExist() throws BBjException {
    when(dispatcher.getCount(NamespaceEvent.class)).thenReturn(1);

    eventSink.removeCallback(null);

    verify(bbjNamespace, times(0)).removeCallbackForNamespace();
  }

  @Test
  void shouldRemoveCallbackWhenNoListenersExist() throws BBjException {
    when(dispatcher.getCount(NamespaceEvent.class)).thenReturn(0);

    eventSink.removeCallback(null);

    verify(bbjNamespace, times(1)).removeCallbackForNamespace();
  }
}
