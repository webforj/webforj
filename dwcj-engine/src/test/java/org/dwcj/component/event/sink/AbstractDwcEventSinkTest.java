package org.dwcj.component.event.sink;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjStaticText;
import com.basis.startup.type.BBjException;
import org.dwcj.component.DwcComponentMock;
import org.dwcj.component.DwcHelperMock;
import org.dwcj.dispatcher.EventDispatcher;
import org.dwcj.component.event.mocks.EventSinkMock;
import org.dwcj.exceptions.DwcjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AbstractDwcEventSinkTest {

  @Mock
  DwcComponentMock component;

  @Mock
  EventDispatcher dispatcher;

  @Mock
  BBjStaticText control;

  @Mock
  DwcHelperMock dwcjHelper;

  @InjectMocks
  EventSinkMock sink;

  @Nested
  @DisplayName("setCallback")
  class SetCallback {
    @Test
    void callSetCallbackOnUnderlyingControl() throws IllegalAccessException, BBjException {
      sink.setCallback();
      verify(control, times(1)).setCallback(eq(0), any(), eq("onEvent"));
    }

    @Test
    void throwDwcjRuntimeExceptionWhenSetCallbackFails()
        throws IllegalAccessException, BBjException {
      doThrow(BBjException.class).when(control).setCallback(eq(0), any(), eq("onEvent"));
      assertThrows(DwcjRuntimeException.class, () -> sink.setCallback());
    }
  }

  @Nested
  @DisplayName("removeCallback")
  class RemoveCallback {
    @Test
    void callClearCallbackOnUnderlyingControl() throws IllegalAccessException, BBjException {
      sink.removeCallback();
      verify(control, times(1)).clearCallback(0);
    }

    @Test
    void throwDwcjRuntimeExceptionWhenClearCallbackFails()
        throws IllegalAccessException, BBjException {
      doThrow(BBjException.class).when(control).clearCallback(0);
      assertThrows(DwcjRuntimeException.class, () -> sink.removeCallback());
    }
  }
}
