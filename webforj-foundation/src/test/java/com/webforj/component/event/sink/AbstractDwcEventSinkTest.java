package com.webforj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjControl;
import com.basis.startup.type.BBjException;
import com.basis.startup.type.CustomObject;
import com.webforj.component.DwcComponentMock;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AbstractDwcEventSinkTest {

  EventDispatcher dispatcher;
  EventSinkMock sink;

  @BeforeEach
  void setUp() {
    dispatcher = new EventDispatcher();
    sink = spy(new EventSinkMock(new DwcComponentMock(), dispatcher, 0));
  }

  @Nested
  @DisplayName("setCallback")
  class SetCallback {
    @Test
    void callSetCallbackOnUnderlyingControl() throws BBjException {
      sink.setCallback();
      verify(sink.getControl(), times(1)).setCallback(anyInt(), any(CustomObject.class),
          anyString());
    }

    @Test
    void throwDwcjRuntimeExceptionWhenSetCallbackFails() throws BBjException {
      BBjControl control = sink.getControl();
      doThrow(BBjException.class).when(control).setCallback(anyInt(), any(CustomObject.class),
          anyString());
      assertThrows(WebforjRuntimeException.class, () -> sink.setCallback());
    }
  }

  @Nested
  @DisplayName("removeCallback")
  class RemoveCallback {
    @Test
    void callClearCallbackOnUnderlyingControl() throws BBjException {
      sink.removeCallback(anyString());
      verify(sink.getControl(), times(1)).clearCallback(0);
    }

    @Test
    @SuppressWarnings("squid:S5778")
    void throwDwcjRuntimeExceptionWhenClearCallbackFails() throws BBjException {
      BBjControl control = sink.getControl();
      doThrow(BBjException.class).when(control).clearCallback(anyInt());
      assertThrows(WebforjRuntimeException.class, () -> sink.removeCallback(anyString()));
    }
  }
}
