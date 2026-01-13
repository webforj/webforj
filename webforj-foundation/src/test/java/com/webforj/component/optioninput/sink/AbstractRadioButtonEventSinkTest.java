package com.webforj.component.optioninput.sink;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import com.basis.bbj.proxies.sysgui.BBjRadioGroup;
import com.basis.startup.type.BBjException;
import com.webforj.component.optioninput.RadioButtonGroup;
import com.webforj.component.optioninput.mocks.RadioButtonEventSinkMock;
import com.webforj.dispatcher.EventDispatcher;
import com.webforj.exceptions.WebforjRuntimeException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class AbstractRadioButtonEventSinkTest {

  @Mock
  RadioButtonGroup component;

  @Spy
  EventDispatcher dispatcher;

  @Mock
  BBjRadioGroup control;

  @InjectMocks
  RadioButtonEventSinkMock sink;

  @Nested
  @DisplayName("setCallback")
  class SetCallback {
    @Test
    void callSetCallbackOnUnderlyingControl() throws BBjException {
      sink.setCallback();
      verify(control, times(1)).setCallback(eq(0), any(), eq("handleEvent"));
    }

    @Test
    void throwDwcjRuntimeExceptionWhenSetCallbackFails() throws BBjException {
      doThrow(BBjException.class).when(control).setCallback(eq(0), any(), eq("handleEvent"));
      assertThrows(WebforjRuntimeException.class, () -> sink.setCallback());
    }
  }

  @Nested
  @DisplayName("removeCallback")
  class RemoveCallback {
    @Test
    void callClearCallbackOnUnderlyingControl() throws BBjException {
      sink.removeCallback(anyString());
      verify(control, times(1)).clearCallback(0);
    }

    @Test
    @SuppressWarnings("squid:S5778")
    void throwDwcjRuntimeExceptionWhenClearCallbackFails() throws BBjException {
      doThrow(BBjException.class).when(control).clearCallback(0);
      assertThrows(WebforjRuntimeException.class, () -> sink.removeCallback(anyString()));
    }
  }
}
