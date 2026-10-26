package com.webforj.testbed.runtime;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.basis.bbj.proxies.BBjSysGui;
import com.basis.bbj.proxies.event.BBjButtonPushEvent;
import com.basis.bbj.proxies.event.BBjTimerEvent;
import com.basis.bbj.proxies.sysgui.BBjButton;
import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.BBjVector;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;

class RuntimeMockTest {
  private static final String CALLBACK_METHOD = "handleEvent";

  static class PlainMock extends RuntimeMock {
  }

  @Test
  void shouldCacheProxiesPerInterface() {
    PlainMock mock = new PlainMock();

    BBjControl first = mock.getProxy(BBjControl.class);
    BBjControl second = mock.getProxy(BBjControl.class);

    assertSame(first, second);
  }

  @Test
  void shouldResolveSymmetricAccessorPairs() throws BBjException {
    PlainMock mock = new PlainMock();
    BBjControl control = mock.getProxy(BBjControl.class);

    control.setText("hello");

    assertEquals("hello", control.getText());
  }

  @Test
  void shouldReturnNeutralDefaults() throws BBjException {
    PlainMock mock = new PlainMock();
    BBjControl control = mock.getProxy(BBjControl.class);

    assertEquals("", control.getText());
    assertFalse(control.isGrouped());
    assertEquals(0, control.getID());
  }

  @Test
  void shouldReturnGenericMocksForRuntimeInterfaces() throws BBjException {
    PlainMock mock = new PlainMock();
    BBjSysGui sysGui = mock.getProxy(BBjSysGui.class);

    assertNotNull(sysGui.getGeolocation());
    assertSame(sysGui.getGeolocation(), sysGui.getGeolocation());
  }

  @Test
  void shouldUseIdentitySemanticsForObjectMethods() {
    PlainMock mock = new PlainMock();
    BBjControl control = mock.getProxy(BBjControl.class);

    assertEquals(control, control);
    assertNotNull(control.toString());
    assertEquals(control.hashCode(), control.hashCode());
  }

  @Test
  void shouldBuildEventsFromReturnValues() {
    BBjButtonPushEvent event =
        RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of("getX", 4d, "getY", 2d));

    assertEquals(4d, event.getX());
    assertEquals(2d, event.getY());
    assertEquals(event, event); // NOSONAR
    assertNotNull(event.toString());
  }

  @Test
  void shouldReturnDefaultsForAbsentEventValues() throws BBjException {
    BBjButtonPushEvent event = RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of());

    assertEquals(0d, event.getX());
    assertNull(event.getControl());
  }

  @Test
  void shouldInvokeCallbacksByName() {
    StringBuilder received = new StringBuilder();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjButtonPushEvent event) {
        received.append("x=").append(event.getX());
      }
    };

    RuntimeMock.Callback callback = new RuntimeMock.Callback(handler, CALLBACK_METHOD);
    callback.invoke(RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of("getX", 7d)));

    assertEquals("x=7.0", received.toString());
  }

  @Test
  void shouldFailClearlyWhenCallbackMethodIsMissing() {
    RuntimeMock.Callback callback = new RuntimeMock.Callback(new Object(), CALLBACK_METHOD);
    BBjButtonPushEvent event = RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of());

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> callback.invoke(event));

    assertTrue(failure.getMessage().contains(CALLBACK_METHOD));
  }

  @Test
  void shouldPropagateCallbackFailures() {
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjButtonPushEvent event) {
        throw new IllegalArgumentException("boom");
      }
    };

    RuntimeMock.Callback callback = new RuntimeMock.Callback(handler, CALLBACK_METHOD);

    BBjButtonPushEvent event = RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of());

    assertThrows(IllegalArgumentException.class, () -> callback.invoke(event));
  }

  @Test
  void shouldForgetTheStoredValueWhenSetToNull() throws BBjException {
    PlainMock mock = new PlainMock();
    BBjControl control = mock.getProxy(BBjControl.class);

    control.setText("hello");
    control.setText(null);

    assertEquals("", control.getText());
  }

  @Test
  void shouldAnswerTheStoredValueOrTheGivenDefault() {
    PlainMock mock = new PlainMock();

    mock.setValue("title", "Orders");

    assertEquals("Orders", mock.getValue("title", "none"));
    assertEquals("none", mock.getValue("missing", "none"));
  }

  @Test
  void shouldTellEventsApartByIdentity() {
    BBjButtonPushEvent first =
        RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of("getX", 4d));
    BBjButtonPushEvent second =
        RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of("getX", 4d));

    assertNotEquals(first, second);
    assertEquals(System.identityHashCode(first), first.hashCode());
  }

  @Test
  void shouldInvokeTheCallbackOverloadAcceptingTheEvent() {
    List<String> received = new ArrayList<>();
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent() {
        received.add("none");
      }

      @SuppressWarnings("unused")
      public void handleEvent(BBjTimerEvent event) {
        received.add("timer");
      }

      @SuppressWarnings("unused")
      public void handleEvent(BBjButtonPushEvent event) {
        received.add("push");
      }
    };

    new RuntimeMock.Callback(handler, CALLBACK_METHOD)
        .invoke(RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of()));

    assertEquals(List.of("push"), received);
  }

  @Test
  void shouldWrapCheckedCallbackFailures() {
    IOException cause = new IOException("unreadable");
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjButtonPushEvent event) throws IOException {
        throw cause;
      }
    };
    RuntimeMock.Callback callback = new RuntimeMock.Callback(handler, CALLBACK_METHOD);
    BBjButtonPushEvent event = RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of());

    IllegalStateException failure =
        assertThrows(IllegalStateException.class, () -> callback.invoke(event));

    assertTrue(failure.getMessage().contains(CALLBACK_METHOD));
    assertSame(cause, failure.getCause().getCause());
  }

  @Test
  void shouldRethrowCallbackErrorsUnchanged() {
    LinkageError error = new LinkageError("broken");
    Object handler = new Object() {
      @SuppressWarnings("unused")
      public void handleEvent(BBjButtonPushEvent event) {
        throw error;
      }
    };
    RuntimeMock.Callback callback = new RuntimeMock.Callback(handler, CALLBACK_METHOD);
    BBjButtonPushEvent event = RuntimeMock.createEvent(BBjButtonPushEvent.class, Map.of());

    assertSame(error, assertThrows(LinkageError.class, () -> callback.invoke(event)));
  }

  @Test
  void shouldReturnEmptyVectorDefault() throws BBjException {
    PlainMock mock = new PlainMock();
    BBjButton button = mock.getProxy(BBjButton.class);

    BBjVector styles = button.getStyles();

    assertNotNull(styles);
    assertTrue(styles.isEmpty());
  }
}
